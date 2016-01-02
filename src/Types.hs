{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
-------------------------------------------------------------------------------
-- |
-- application types
--------------------------------------------------------------------------------
module Types where

import           Control.Monad              (mzero)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Data
import           Data.Time                  (Day, TimeOfDay)
import           Data.Time.Calendar         (toGregorian)
import           Data.Time.Format           (defaultTimeLocale,
                                             parseTimeOrError)
import           Data.Time.LocalTime        (LocalTime, localDay)
import           Data.Typeable
import           Data.Yaml

type Year = Integer
type Month = Int

-- | CMS Login
data LoginData = LoginData {
    ldUser     :: String
  , ldPassword :: String
} deriving (Show)

instance FromJSON LoginData where
    parseJSON (Object v) = LoginData <$>
                           v .: "user" <*>
                           v .: "pass"

    parseJSON _ = mzero

data EventTemplate = EventTemplate {
      etTitle    :: String
    , etTime     :: TimeOfDay
    , etType     :: String
    , etDesc     :: String
    , etUrl      :: String
    , etCalTitle :: String
                  } deriving (Show)

instance FromJSON EventTemplate where
    parseJSON (Object v) = EventTemplate <$>
                           v .: "title" <*>
                           (parseTime <$> v .: "time") <*>
                           v .: "type" <*>
                           v .: "desc" <*>
                           v .: "url" <*>
                           v .: "calendar-title"
        where parseTime = parseTimeOrError True defaultTimeLocale "%T" :: String -> TimeOfDay

    parseJSON _ = mzero

data ReminderMailConfig = ReminderMailConfig {
      rmcEnabled   :: Bool
    , rmcDaysAhead :: Int
    , rmcHost      :: String
    , rmcUser      :: String
    , rmcPass      :: String
    , rmcSender    :: String
    , rmcReceiver  :: String
    , rmcSubject   :: String
    , rmcBody      :: String
    } deriving (Show)


instance FromJSON ReminderMailConfig where
    parseJSON (Object v) = ReminderMailConfig <$>
                           v .: "enabled" <*>
                           v .: "days-ahead" <*>
                           v .: "host" <*>
                           v .: "login-user" <*>
                           v .: "login-pass" <*>
                           v .: "sender" <*>
                           v .: "receiver" <*>
                           v .: "subject" <*>
                           v .: "body"

    parseJSON _ = mzero


data Config = Config {
      loginData          :: LoginData
    , eventTemplate      :: EventTemplate
    , reminderMailConfig :: ReminderMailConfig
    } deriving (Show)


instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "cms-login" <*>
                           v .: "event-template" <*>
                           v .: "reminder-mail"


-- | Chaostreff event
data Event = Event {
    eTitle    :: String
  , eDate     :: LocalTime
  , eType     :: String
  , eDesc     :: String
  , eUrl      :: String
  , eCalTitle :: String
} deriving (Show, Data, Typeable)


type App a = ReaderT Config (ExceptT AppError IO) a

-- | Successful application run result
data SchedulingResult = AlreadyScheduled Day
                      | EventScheduled Event String
                      deriving Show

-- | Error application run result
data AppError = LoginError String
              | ParseResponseBodyError BL.ByteString
              | ExtractAlertError
              | EventTableNotFoundError
              | EventTableBodyNotFoundError
              | EventTableRowsNotFoundError
              | EventTitleNotFoundError
              | EventDateParseError String
              | InvalidConfig String
              | SendReminderError IOError
              deriving Show


class IsIn a where
    isInMonth :: Year -> Month -> a -> Bool
    isInDay :: Day -> a -> Bool

instance IsIn Event where
    isInMonth y m e = let (y', m', _) = toGregorian $ localDay (eDate e)
                      in y == y' && m == m'

    isInDay d e = d == (localDay . eDate) e

