{-# LANGUAGE ViewPatterns #-}
module Scheduling where

import           CMSCalendar
import           Control.Monad
import           Data.Time
import           Upcoming

{-
scheduleThisMonth :: IO (Either String String)
scheduleThisMonth = do
  (y, m, _) <- toGregorian . utctDay <$> getCurrentTime
  scheduleEventsAt y m


scheduleEventsAt :: Year -> Month -> IO (Either String String)
scheduleEventsAt y m = do
  scheduled <- scheduledEventsAt y m
  postEvents $ diff scheduled (upcomingAt y m)
-}



-- |
--
--
-- >>> diff [2,4] [1,2,3,4,5]
-- [1,3,5]
diff :: Eq a => [a] -> [a] -> [a]
diff toRemove = filter (not . flip elem toRemove)
