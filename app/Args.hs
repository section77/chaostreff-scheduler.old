module Args where

import           Options.Applicative


data AppArgs = ScheduleThisMonth {
                 user :: String
               , pass :: String
               }
             | ScheduleMonth {
                 user  :: String
               , pass  :: String
               , year  :: Integer
               , month :: Int
               }


appArgs :: Parser AppArgs
appArgs = ScheduleThisMonth
                  <$> argument str (metavar "<USER>")
                  <*> argument str (metavar "<PASS>")
          <|> ScheduleMonth
              <$> argument str (metavar "<USER>")
              <*> argument str (metavar "<PASS>")
              <*> argument auto (metavar "<YEAR>")
              <*> argument auto (metavar "<MONTH>")
