{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
-- schedule chaostreff events
--
-- this is the entry point for the 'chaostreff-scheduler' library
--------------------------------------------------------------------------------
module Scheduling (
    scheduleEvents
  , scheduleEventsAt
) where

import           CMSCalendar
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (asks)
import           Data.Time
import           Types
import           Upcoming



-- | schedule chaostreff events for the current year / month
scheduleEvents :: App [SchedulingResult]
scheduleEvents = do
  ld <- asks loginData
  (y, m, _) <- lift . lift $ toGregorian . utctDay <$> getCurrentTime
  scheduleEventsAt y m



-- | schedule chaostreff events for the given year / month
scheduleEventsAt :: Year -> Month -> App [SchedulingResult]
scheduleEventsAt y m = do
  et <- asks eventTemplate
  scheduled <- scheduledEventsAt y m
  mapM (postIfUnscheduled et scheduled) $ upcomingAt y m
    where postIfUnscheduled et scheduled day =
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
