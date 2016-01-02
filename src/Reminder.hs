{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
--------------------------------------------------------------------------------
module Reminder (sendReminders) where

import           CMSCalendar                (scheduledEvents)
import           Control.Monad              (when)
import           Control.Monad.Catch        (catchIOError, try)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (throwE)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Time                  (LocalTime (..), TimeOfDay (..),
                                             addDays, getCurrentTime, utctDay)
import           Data.Time.Calendar         (toGregorian)
import           Network.Mail.SMTP
import           Text.StringTemplate
import           Types


type Receiver = String
type Body = TL.Text


sendReminders :: App ()
sendReminders = do
  enabled <- asks $ rmcEnabled . reminderMailConfig
  daysAhead <- asks $ rmcDaysAhead . reminderMailConfig
  when enabled $ do
                  events <- scheduledEvents
                  day <- lift . lift $ addDays (fromIntegral daysAhead) . utctDay <$> getCurrentTime
                  mapM_ sendRemindersForEvent $ filter (isInDay day) events




sendRemindersForEvent :: Event -> App ()
sendRemindersForEvent e = do
  rs <- asks $ rmcReceiver . reminderMailConfig
  mapM (sendReminderForEvent e) [rs]
  return ()


sendReminderForEvent :: Event -> Receiver -> App ()
sendReminderForEvent e r = do
  rmc <- asks reminderMailConfig
  let from = Address Nothing $ T.pack (rmcSender rmc)
  let to = Address Nothing $ T.pack r
  let subject = T.pack . rmcSubject $ rmc
  let body = plainTextPart $ enrich (newSTMP (rmcBody rmc)) e r
  let send = lift $ sendMail (rmcHost rmc) $ simpleMail from [to] [] [] subject [body]
  lift $ catchIOError send (throwE . SendReminderError)


-- |
--
-- >>> let t = newSTMP "title: $title$, date: $day$.$month$.$year$ $hour$:$minute$"
-- >>> let e = Event "e-title" (read "2016-01-01 11:30:00") "e-type" "e-desc" "e-url" "e-cal-title"
-- >>> enrich t e ""
-- "title: e-title, date: 01.01.2016 11:30"
enrich :: (StringTemplate String) -> Event -> Receiver -> Body
enrich t e r = let (year, month, day) = toGregorian . localDay . eDate $ e
                   TimeOfDay hour minute second = localTimeOfDay . eDate $ e
               in TL.pack . render $ setManyAttrib [
                       ("title", eTitle e)
                      ,("type", eType e)
                      ,("desc", eDesc e)
                      ,("url", eUrl e)
                      ,("cal-title", eCalTitle e)
                      ,("year", lpad 4 year)
                      ,("month", lpad 2 month)
                      ,("day", lpad 2 day)
                      ,("hour", lpad 2 hour)
                      ,("minute", lpad 2 minute)
                      ,("second", lpad 2 second)
                      ,("receiver", r)
                      ] t
    where  lpad l n = let s = "0000" ++ show n
                    in drop (length s - l) s

