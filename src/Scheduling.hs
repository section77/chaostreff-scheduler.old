{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
-- schedule chaostreff events
--------------------------------------------------------------------------------
module Scheduling (
    scheduleEvents
  , scheduleEventsAt
) where

import           CMSCalendar
import           Data.Maybe          (fromMaybe, listToMaybe)
import           Data.Time
import           Data.Time.LocalTime (utcToLocalTime)
import           Types
import           Upcoming
import           Utils


-- | schedule chaostreff events for the current year / month
scheduleEvents :: LoginData -> IO (Either String String)
scheduleEvents loginData = do
  (y, m, _) <- toGregorian . utctDay <$> getCurrentTime
  fromMaybe (Left "nothing to do") . listToMaybe <$> scheduleEventsAt loginData y m


-- | schedule chaostreff events for the given year / month
scheduleEventsAt :: LoginData -> Year -> Month -> IO [Either String String]
scheduleEventsAt loginData y m = do
  Right scheduled <- scheduledEventsAt loginData y m
  let days = diffBy (flip isInDay) scheduled (upcomingAt y m)
  let events = map newEvent days
  mapM (postEvent loginData) events



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
