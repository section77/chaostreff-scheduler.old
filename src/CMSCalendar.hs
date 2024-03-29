{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
-- interface to the cms calendar
--------------------------------------------------------------------------------
module CMSCalendar (
    scheduledEvents
  , scheduledEventsAt
  , postEvent
) where

import           Control.Lens
import           Control.Monad.Trans.Except      (ExceptT (..))
import qualified Data.ByteString.Lazy.Char8      as BL
import           Data.List                       (isInfixOf)
import           Data.Time.Format                (defaultTimeLocale, formatTime,
                                                  parseTimeM)
import           Network.Connection              (TLSSettings (..))
import qualified Network.HTTP.Client             as HTTP
import           Network.HTTP.Client.TLS         (mkManagerSettings)
import           Network.Wreq.StringLess
import qualified Network.Wreq.StringLess.Session as S
import           Network.Wreq.StringLess.Types   (Postable (..))
import           Protolude                       hiding ((&))
import           Text.XML.Light
import           Types
import           Utils



-- * exported functions



-- | already scheduled events in the cms calendar
scheduledEvents :: App [Event]
scheduledEvents = withCMSSession $ \sess -> do
                    res <- lift $ S.getWith opts sess scheduledEventsUrl
                    body <- return $ parseResponseBody (res ^. responseBody)
                    liftEither $ body >>= extractEvents


-- | already scheduled events in the cms calendar for a given year and month
scheduledEventsAt :: Year -> Month -> App [Event]
scheduledEventsAt y m = fmap (filter $ isInMonth y m) scheduledEvents


-- | post a event to the cms calendar
postEvent :: Event -> App [Char]
postEvent e = withCMSSession $ \sess -> do
                res <- lift $ S.postWith opts sess postEventUrl e
                liftEither $ parseResponseBody (res ^. responseBody) >>= extractAlert



-- * private functions

-- | cms login url
loginUrl :: ByteString
loginUrl = "https://cms.section77.de/index.php/login/do_login/"

-- | cms scheduled events list url
scheduledEventsUrl :: ByteString
scheduledEventsUrl = "https://cms.section77.de/index.php/dashboard/event_calendar/list_event/"

-- | cms post event url
postEventUrl :: ByteString
postEventUrl = "https://cms.section77.de/index.php/dashboard/event_calendar/event/"

-- | HTTP ManagerSettings with certificate validation disabled
managerSettings :: HTTP.ManagerSettings
managerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing

-- | wreq request options
opts :: Options
opts = defaults & manager .~ Left managerSettings


-- | runs the given function in a cms session
--
-- * handels user login
-- * verify the login was successful
withCMSSession :: (S.Session -> ExceptT AppError IO a) -> App a
withCMSSession f = do
  ld <- asks loginData
  lift $ withSession $ \sess -> do
      _ <- lift $ S.getWith opts sess loginUrl -- cms required this. it expects a cookie in the login post request!?!?!
      res <- lift $ S.postWith opts sess loginUrl ld
      either (const $ f sess) (throwError . LoginError) $ parseResponseBody (res ^. responseBody) >>= extractAlert
    where -- lift S.withSession in ExpectT AppError IO a
          withSession :: (S.Session -> ExceptT AppError IO a) -> ExceptT AppError IO a
          withSession f = ExceptT $ S.withSessionControl (Just (HTTP.createCookieJar [])) managerSettings (runExceptT . f)




-- * parse html / extract elements

parseResponseBody :: BL.ByteString -> Either AppError Element
parseResponseBody body = case parseXMLDoc body of
  Just xml -> Right xml
  Nothing  -> Left $ ParseResponseBodyError body


extractAlert :: Element -> Either AppError [Char]
extractAlert e = maybe (Left ExtractAlertError) Right $  strContent <$> filterElement (hasAttrVal "class" "alert") e

-- |
-- FIXME: one fail -> all fail
extractEvents :: Element -> Either AppError [Event]
extractEvents e = do
  table <- maybeToRight EventTableNotFoundError $ filterElement (hasAttrVal "id" "listevent") e
  tbody <- maybeToRight EventTableBodyNotFoundError $ filterChildName (isElementOf "tbody") table
  trs <- maybeToRight EventTableRowsNotFoundError  $ Just (filterChildrenName (isElementOf "tr") tbody)
  sequence $ map parseEvent trs


parseEvent :: Element -> Either AppError Event
parseEvent e = let [title, date, type', desc, url, calTitle, _] = elChildren e
                   parsedDate = maybeToRight (EventDateParseError (strContent date)) $ parseTimeM True defaultTimeLocale "%F %T" (strContent date)
                in Event <$> (maybeToRight EventTitleNotFoundError $ strContent <$> filterChildName (isElementOf "input") title)
                         <*> parsedDate
                         <*> Right (strContent type')
                         <*> Right (strContent desc)
                         <*> Right (strContent url)
                         <*> Right (strContent calTitle)




isElementOf :: [Char] -> QName -> Bool
isElementOf tag (QName n _ _) = n == tag


hasAttrVal :: [Char] -> [Char] -> Element -> Bool
hasAttrVal attr val e = maybe False (isInfixOf val) $ findAttr (unqual attr) e


instance Postable LoginData where
    postPayload (LoginData u p) = postPayload ["uName" := u, "uPassword" := p]


instance Postable Event where
    postPayload e = postPayload ["event_title" := eTitle e,
                                 "event_calendarID" := ("3" :: [Char]), -- 3 -> Chaostreff
                                 "event_date" := formatTime defaultTimeLocale "%F+%T" (eDate e),
                                 "event_type" := eType e,
                                 "event_description" := eDesc e,
                                 "event_url" := eUrl e]
