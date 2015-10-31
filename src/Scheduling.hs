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
import           Control.Monad.Trans.Class (lift)
import           Data.Time
import           Types
import           Upcoming



-- | schedule chaostreff events for the current year / month
scheduleEvents :: LoginData -> AppResult [SchedulingResult]
scheduleEvents loginData = do
  (y, m, _) <- lift $ toGregorian . utctDay <$> getCurrentTime
  scheduleEventsAt loginData y m



-- | schedule chaostreff events for the given year / month
scheduleEventsAt :: LoginData -> Year -> Month -> AppResult [SchedulingResult]
scheduleEventsAt loginData y m = do
  scheduled <- scheduledEventsAt loginData y m
  mapM (postIfUnscheduled scheduled) $ upcomingAt y m
    where postIfUnscheduled scheduled day =
              if any (day `isInDay`) scheduled then
                  lift . return $ AlreadyScheduled day
              else
                  let event = newEvent day
                  in EventScheduled event <$> (postEvent loginData event)



-- | FIXME: event template from configuration file
newEvent :: Day -> Event
newEvent day = let time = TimeOfDay {
                          todHour = 20
                        , todMin = 0
                        , todSec = 0
                        }
             in Event {
                  title = "Chaostreff"
                , date = LocalTime day time
                , type' = "Chaostreff im Hackspace"
                , desc = "Mittwochstreffen des Vereins im Hackspace (wie jeden 2. und 4. Mittwoch des Monats)"
                , url = "http://osm.org/go/0DLdM4FF2--?way=297085686"
                , calTitle = "Chaostreff"
                }
