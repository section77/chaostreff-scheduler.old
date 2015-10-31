{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
-- application types
--------------------------------------------------------------------------------
module Types where

import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Time                  (Day)
import           Data.Time.Calendar         (toGregorian)
import           Data.Time.LocalTime        (LocalTime, localDay)

type Year = Integer
type Month = Int

-- | CMS Login
data LoginData = LoginData {
    user     :: String
  , password :: String
} deriving Show


-- | Chaostreff event
data Event = Event {
    title    :: String
  , date     :: LocalTime
  , type'    :: String
  , desc     :: String
  , url      :: String
  , calTitle :: String
} deriving Show

-- | Result of the application run
type AppResult a = ExceptT AppError IO a

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
              deriving Show


class IsIn a where
    isInMonth :: Year -> Month -> a -> Bool
    isInDay :: Day -> a -> Bool

instance IsIn Event where
    isInMonth y m e = let (y', m', _) = toGregorian $ localDay (date e)
                      in y == y' && m == m'

    isInDay d e = d == (localDay . date) e

