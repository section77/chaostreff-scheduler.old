{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- interface to the cms calendar
--------------------------------------------------------------------------------
module CMSCalendar (
    scheduledEventsAt
  , postEvent
) where

import           Control.Lens
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



tlsTestOk = let opts = defaults & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
            in getWith opts "https://cms.section77.de/index.php/login/"

tlsTestNok = let opts = defaults & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
             in S.withSession $ \sess -> S.getWith opts sess "https://cms.section77.de/index.php/login/"




-- * exported functions

-- | already scheduled events in the cms calendar for a given year and month
scheduledEventsAt :: LoginData -> Year -> Month -> IO (Either String [Event])
scheduledEventsAt loginData y m = withCMSSession loginData $ \sess -> do
                          res <- S.get sess "http://cms.section77.de/index.php/dashboard/event_calendar/list_event/"
                          return $ parseResponseBody (res ^. responseBody) >>= \e -> fmap (filter $ isInMonth y m) $ extractEvents e


-- | post a event to the cms calendar
postEvent :: LoginData -> Event -> IO (Either String String)
postEvent loginData e = withCMSSession loginData $ \sess -> do
                          res <- S.post sess "http://cms.section77.de/index.php/dashboard/event_calendar/event/" e
                          return $ parseResponseBody (res ^. responseBody) >>= maybeToEither "msg not found" . extractAlert





-- * private functions



withCMSSession :: LoginData -> (S.Session -> IO a) -> IO a
withCMSSession loginData f = S.withSession $ \sess ->
        do _ <- S.get sess "http://cms.section77.de/index.php/login/"
           res <- S.post sess "http://cms.section77.de/index.php/login/do_login/" loginData
           -- FIXME: change type to include error
           let Right loginMsg = extractAlert <$> parseResponseBody (res ^. responseBody)
           _ <- maybe (return ()) print loginMsg
           f sess



parseResponseBody :: BL.ByteString -> Either String Element
parseResponseBody x = case parseXMLDoc x of
  Just xml -> Right xml
  Nothing  -> Left "parse html resposne error"


extractAlert :: Element -> Maybe String
extractAlert e = strContent <$> filterElement (hasAttrVal "class" "alert") e

-- |
-- FIXME: one fail -> all fail
extractEvents :: Element -> Either String [Event]
extractEvents e = do
  table <- maybeToEither "event table not found" $ filterElement (hasAttrVal "id" "listevent") e
  tbody <- maybeToEither "event table-body not found" $ filterChildName (isElementOf "tbody") table
  trs <- maybeToEither "event rows not found" $ Just (filterChildrenName (isElementOf "tr") tbody)
  sequence $ map parseEvent trs


parseEvent :: Element -> Either String Event
parseEvent e = let [title, date, type', desc, url, calTitle, _] = elChildren e
                   parsedDate = parseTimeM True defaultTimeLocale "%F %T" (strContent date) :: Either String LocalTime
                in Event <$> (maybeToEither "event title not found" $ strContent <$> filterChildName (isElementOf "input") title)
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



