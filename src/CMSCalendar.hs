{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
-- interface to the cms calendar
--------------------------------------------------------------------------------
module CMSCalendar (
    scheduledEventsAt
  , postEvent
) where

import           Control.Lens
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                  (isInfixOf)
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             parseTimeM)
import           Data.Time.LocalTime        (LocalTime (..))
import           Network.Connection         (TLSSettings (..))
import           Network.HTTP.Client.TLS    (mkManagerSettings)
import           Network.Wreq
import qualified Network.Wreq.Session       as S
import           Network.Wreq.Types         (Postable (..))
import           Text.XML.Light
import           Types
import           Utils



-- * exported functions


-- | already scheduled events in the cms calendar for a given year and month
scheduledEventsAt :: LoginData -> Year -> Month -> AppResult [Event]
scheduledEventsAt loginData y m = withCMSSession loginData $ \sess -> do
                          res <- lift $ S.get sess scheduledEventsUrl
                          body <- return $ parseResponseBody (res ^. responseBody)
                          liftEither $ body >>= fmap (filter $ isInMonth y m) . extractEvents


-- | post a event to the cms calendar
postEvent :: LoginData -> Event -> AppResult String
postEvent loginData e = withCMSSession loginData $ \sess -> do
                          res <- lift $ S.post sess postEventUrl e
                          liftEither $ parseResponseBody (res ^. responseBody) >>= extractAlert





-- * private functions

loginUrl = "http://cms.section77.de/index.php/login/do_login/"
scheduledEventsUrl = "http://cms.section77.de/index.php/dashboard/event_calendar/list_event/"
postEventUrl = "http://cms.section77.de/index.php/dashboard/event_calendar/event/"

opts = defaults & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)


-- | runs the given function in a cms session
--
-- * handels user login
-- * verify the login was successful
withCMSSession :: LoginData -> (S.Session -> AppResult a) -> AppResult a
withCMSSession loginData f = withSession $ \sess -> do
      _ <- lift $ S.get sess loginUrl -- cms required this. it expects a cookie in the login post request!?!?!
      res <- lift $ S.post sess loginUrl loginData
      either (const $ f sess) (throwE . LoginError) $ parseResponseBody (res ^. responseBody) >>= extractAlert
    where -- lift S.withSession in AppResult
          withSession :: (S.Session -> AppResult a) -> AppResult a
          withSession f = ExceptT $ S.withSession (runExceptT . f)




-- * parse html / extract elements

parseResponseBody :: BL.ByteString -> Either AppError Element
parseResponseBody body = case parseXMLDoc body of
  Just xml -> Right xml
  Nothing  -> Left $ ParseResponseBodyError body


extractAlert :: Element -> Either AppError String
extractAlert e = maybe (Left ExtractAlertError) Right $  strContent <$> filterElement (hasAttrVal "class" "alert") e

-- |
-- FIXME: one fail -> all fail
extractEvents :: Element -> Either AppError [Event]
extractEvents e = do
  table <- maybeToEither EventTableNotFoundError $ filterElement (hasAttrVal "id" "listevent") e
  tbody <- maybeToEither EventTableBodyNotFoundError $ filterChildName (isElementOf "tbody") table
  trs <- maybeToEither EventTableRowsNotFoundError  $ Just (filterChildrenName (isElementOf "tr") tbody)
  sequence $ map parseEvent trs


parseEvent :: Element -> Either AppError Event
parseEvent e = let [title, date, type', desc, url, calTitle, _] = elChildren e
                   parsedDate = maybeToEither (EventDateParseError (strContent date)) $ parseTimeM True defaultTimeLocale "%F %T" (strContent date)
                in Event <$> (maybeToEither EventTitleNotFoundError $ strContent <$> filterChildName (isElementOf "input") title)
                         <*> parsedDate
                         <*> Right (strContent type')
                         <*> Right (strContent desc)
                         <*> Right (strContent url)
                         <*> Right (strContent calTitle)




isElementOf :: String -> QName -> Bool
isElementOf tag (QName n _ _) = n == tag


hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal attr val e = maybe False (isInfixOf val) $ findAttr (unqual attr) e


instance Postable LoginData where
    postPayload (LoginData u p) = postPayload ["uName" := u, "uPassword" := p]


instance Postable Event where
    postPayload e = postPayload ["event_title" := title e,
                                 "event_calendarID" := ("3" :: String), -- 3 -> Chaostreff
                                 "event_date" := formatTime defaultTimeLocale "%F+%T" (date e),
                                 "event_type" := type' e,
                                 "event_description" := desc e,
                                 "event_url" := url e]
