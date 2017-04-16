{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Time                  (Day, TimeOfDay)
import           Data.Time.Calendar         (toGregorian)
import           Data.Time.Format           (defaultTimeLocale,
                                             parseTimeOrError)
import           Data.Time.LocalTime        (LocalTime, localDay)
import           Data.Yaml
import           Protolude

type Year = Integer
type Month = Int

-- | CMS Login
data LoginData = LoginData {
    ldUser     :: [Char]
  , ldPassword :: [Char]
} deriving (Show)

instance FromJSON LoginData where
    parseJSON (Object v) = LoginData <$>
                           v .: "user" <*>
                           v .: "pass"

    parseJSON _ = mzero

data EventTemplate = EventTemplate {
      etTitle    :: [Char]
    , etTime     :: TimeOfDay
    , etType     :: [Char]
    , etDesc     :: [Char]
    , etUrl      :: [Char]
    , etCalTitle :: [Char]
                  } deriving (Show)

instance FromJSON EventTemplate where
    parseJSON (Object v) = EventTemplate <$>
                           v .: "title" <*>
                           (parseTime <$> v .: "time") <*>
                           v .: "type" <*>
                           v .: "desc" <*>
                           v .: "url" <*>
                           v .: "calendar-title"
        where parseTime = parseTimeOrError True defaultTimeLocale "%T" :: [Char] -> TimeOfDay

    parseJSON _ = mzero

data Config = Config {
      loginData     :: LoginData
    , eventTemplate :: EventTemplate
    } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "cms-login" <*>
                           v .: "event-template"


-- | Chaostreff event
data Event = Event {
    eTitle    :: [Char]
  , eDate     :: LocalTime
  , eType     :: [Char]
  , eDesc     :: [Char]
  , eUrl      :: [Char]
  , eCalTitle :: [Char]
} deriving Show


type App a = ReaderT Config (ExceptT AppError IO) a

-- | Successful application run result
data SchedulingResult = AlreadyScheduled Day
                      | EventScheduled Event [Char]
                      deriving Show

-- | Error application run result
data AppError = LoginError [Char]
              | ParseResponseBodyError LByteString
              | ExtractAlertError
              | EventTableNotFoundError
              | EventTableBodyNotFoundError
              | EventTableRowsNotFoundError
              | EventTitleNotFoundError
              | EventDateParseError [Char]
              | InvalidConfig [Char]
              deriving Show


class IsIn a where
    isInMonth :: Year -> Month -> a -> Bool
    isInDay :: Day -> a -> Bool

instance IsIn Event where
    isInMonth y m e = let (y', m', _) = toGregorian $ localDay (eDate e)
                      in y == y' && m == m'

    isInDay d e = d == (localDay . eDate) e

