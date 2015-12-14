module Args where

import           Options.Applicative
import           Types               (Month, Year)

data AppArgs = ShowVersion
             | ScheduleThisMonth {
                 cfgFile :: FilePath
               }
             | ScheduleMonth {
                 cfgFile :: FilePath
               , year    :: Year
               , month   :: Month
               }



appArgs :: Parser AppArgs
appArgs = flag' ShowVersion (short 'v' <> long "version" <> help "chaostreff scheduler version")
          <|> ScheduleMonth
                  <$> argument str (metavar "<CFG>")
                  <*> argument auto (metavar "<YEAR>")
                  <*> argument auto (metavar "<MONTH>")
          <|> ScheduleThisMonth
                  <$> argument str (metavar "<CFG>")
