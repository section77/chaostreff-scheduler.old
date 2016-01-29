{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- schedule chaostreff events
--
-- this is the entry point for the 'chaostreff-scheduler' library
--------------------------------------------------------------------------------
module Scheduling (
    scheduleNextEvents
  , scheduleEventsAt
) where

import           CMSCalendar
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (asks)
import           Data.Time
import           Data.Time.Calendar         (addGregorianMonthsClip)
import           Types
import           Upcoming



-- | schedule chaostreff events for the next n months
scheduleNextEvents :: Int -> App [SchedulingResult]
scheduleNextEvents n = do
  et <- asks eventTemplate
  scheduled <- scheduledEvents
  today <- lift . lift $ utctDay <$> getCurrentTime
  let days = concatMap upcoming $ map (flip addGregorianMonthsClip today . fromIntegral) [0..(n-1)]
  mapM (postIfUnscheduled et scheduled) days
    where upcoming (toGregorian -> (y, m, _)) = upcomingAt y m


-- | schedule chaostreff events for the given year / month
scheduleEventsAt :: Year -> Month -> App [SchedulingResult]
scheduleEventsAt y m = do
  et <- asks eventTemplate
  scheduled <- scheduledEventsAt y m
  mapM (postIfUnscheduled et scheduled) $ upcomingAt y m



postIfUnscheduled :: EventTemplate -> [Event] -> Day -> App SchedulingResult
postIfUnscheduled et scheduled day =
    if any (day `isInDay`) scheduled then
        lift . return $ AlreadyScheduled day
    else
        let event = Event {
                      eTitle = etTitle et
                    , eDate = LocalTime day (etTime et)
                    , eType = etType et
                    , eDesc = etDesc et
                    , eUrl = etUrl et
                    , eCalTitle = etCalTitle et
                    }
        in EventScheduled event <$> postEvent event
