{-# LANGUAGE OverloadedStrings #-}
module CMSCalendar (
    scheduledEventsAt
  , postEvents
) where

import           Control.Lens
import qualified Data.ByteString.Char8   as B
import           Data.Text               (strip)
import           Data.Time               (Day)
import           Data.Time.Format        (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime     (LocalTime (..))
import           Network.Connection      (TLSSettings (..))
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.Wreq
import qualified Network.Wreq.Session    as S
import           Text.XML.Light

type Year = Integer
type Month = Int



tlsTestOk = let opts = defaults & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
            in getWith opts "https://cms.section77.de/index.php/login/"

tlsTestNok = let opts = defaults & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
             in S.withSession $ \sess -> S.getWith opts sess "https://cms.section77.de/index.php/login/"

-- OverloadedStrings ????
loginData = ["uName" := ("<USER>" :: String), "uPassword" := ("<PASS>" :: String), "rcID" := ("" :: String), "submit" := ("Anmelden+>" :: String)]


scheduledEventsAt :: Year -> Month -> IO (Either String [Event])
scheduledEventsAt y m = S.withSession $ \sess -> do
                          _ <- S.get sess "http://cms.section77.de/index.php/login/"
                          l <- S.post sess "http://cms.section77.de/index.php/login/do_login/" loginData
                          case l ^. responseStatus . statusCode of
                            200 -> do
                              c <- S.get sess "http://cms.section77.de/index.php/dashboard/event_calendar/list_event/"
                              return $ parseResponseBody (c ^. responseBody)
                            code -> return $ Left "login failed"


-- OverloadedStrings ????
fd = ["event_title" := ("ketest" :: String), "event_calendarID" := ("3":: String), "event_date" := ("2016-01-01+23:00:54"::String), "event_type" := ("test" :: String), "event_description" := ("test" :: String), "event_url" := (""::String) ]


postEvents :: [Day] -> IO (Either String String)
postEvents days = S.withSession $ \sess -> do
                      _ <- S.get sess "http://cms.section77.de/index.php/login/"
                      l <- S.post sess "http://cms.section77.de/index.php/login/do_login/" loginData
                      case l ^. responseStatus . statusCode of
                        200 -> do
                          r <- S.post sess "http://cms.section77.de/index.php/dashboard/event_calendar/event/" fd
                          return $ Right (show $ r ^. responseStatus . statusCode)
                        code -> return $ Left "login failed"



parseResponseBody x = case parseXMLDoc x of
  Just xml -> extractEvents xml
  Nothing  -> Left "parse html resposne error"



data Event = Event {
    title       :: String
  , date        :: LocalTime
  , type'       :: String
  , description :: String
  , url         :: String
  , calTitle    :: String
} deriving Show


-- |
-- FIXME: one fail -> all fail
extractEvents :: Element -> Either String [Event]
extractEvents e = do
  table <- toEither "event table not found" $ filterElement (hasAttrVal "id" "listevent") e
  tbody <- toEither "event table-body not found" $ filterChildName (isElementOf "tbody") table
  trs <- toEither "event rows not found" $ Just (filterChildrenName (isElementOf "tr") tbody)
  sequence $ map parseEvent trs


parseEvent :: Element -> Either String Event
parseEvent e = let [title, date, type', desc, url, calTitle, _] = elChildren e
                   parsedDate = parseTimeM True defaultTimeLocale "%F %T" (strContent date) :: Maybe LocalTime
                in Event <$> (toEither "event title not found" $ strContent <$> filterChildName (isElementOf "input") title)
                         <*> (toEither "date not parsable" parsedDate)
                         <*> Right (strContent type')
                         <*> Right (strContent desc)
                         <*> Right (strContent url)
                         <*> Right (strContent calTitle)


toEither :: a -> Maybe b -> Either a b
toEither a Nothing  = Left a
toEither _ (Just b) = Right b


isElementOf :: String -> QName -> Bool
isElementOf tag (QName n _ _) = n == tag


hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal attr val e = findAttr (unqual attr) e == Just val
