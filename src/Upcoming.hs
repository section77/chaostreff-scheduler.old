{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
-- calculates upcoming events
--------------------------------------------------------------------------------
module Upcoming (
    upcomingAt
) where

import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Protolude
import           Types
import           Unsafe



-- * exported functions

-- | upcoming events in a given year and month
--
-- >>> upcomingAt 2016 01
-- [2016-01-13,2016-01-27]
upcomingAt :: Year -> Month -> [Day]
upcomingAt y m = oddElements tuesdays
    where oddElements = map snd . filter (even . fst) . zip [1..]
          tuesdays = filter isTuesday $ allDaysAt y m



-- * private functions

-- | all days in a given year and month
allDaysAt :: Year -> Month -> [Day]
allDaysAt y m = map toDay daysOfMonth
    where toDay = fromGregorian y m
          daysOfMonth = take (daysInMonth m) [1..]



-- | number of days in a month
--
--  * for n <= 1 january day count
--  * for n >= 12 december day count
--
-- >>> daysInMonth 1
-- 31
--
-- >>> daysInMonth 2
-- 28
--
daysInMonth :: Month -> Int
daysInMonth n
    | n <= 1    = unsafeHead days
    | n >= 12   = unsafeLast days
    | otherwise = unsafeIndex days (n - 1)
    where days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]



-- | evaluates to True if the given day is a tuesday
--
-- >>> -- the 01.01.2016 is a friday
-- >>> isTuesday $ fromGregorian 2016 01 01
-- False
--
-- >>> -- the 05.01.2016 is a wednesday
-- >>> isTuesday $ fromGregorian 2016 01 05
-- True
--
isTuesday :: Day -> Bool
isTuesday = (==) 2 . dayOfWeek



-- | day of week for a given day
--
-- 1 for Monday to 7 for Sunday
--
-- >>> -- the 01.01.2016 is a friday
-- >>> dayOfWeek $ fromGregorian 2016 01 01
-- 5
--
dayOfWeek :: Day -> Int
dayOfWeek (toWeekDate -> (_, _, d)) = d
