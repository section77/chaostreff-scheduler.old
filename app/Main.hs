module Main where

import           Args
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.List                  (concat, intersperse)
import           Data.Version               (showVersion)
import           Data.Yaml
import           Options.Applicative        (execParser, fullDesc, header,
                                             helper, info, progDesc, (<>))
import           Paths_chaostreff_scheduler (version)
import           Scheduling
import           Types


main :: IO ()
main = execParser opts >>= run
    where opts = info (helper <*> appArgs)
                 ( fullDesc
                 <> progDesc "call it with the config file, to schedule the actual month, call it with the config file and a year and month to schedule for the given year and month"
                 <> header "chaostreff-scheduler - schedule chaostreff events")


run :: AppArgs -> IO ()
run ShowVersion = putStrLn $ showVersion version
run (ScheduleThisMonth cfgFile) = printResult $ runApp scheduleEvents cfgFile
run (ScheduleMonth cfgFile y m) = printResult $ runApp (scheduleEventsAt y m) cfgFile


runApp :: App [SchedulingResult] -> FilePath -> ExceptT AppError IO [SchedulingResult]
runApp app cfgFile = do
  errOrCfg <- lift $ decodeFileEither cfgFile
  either (throwE . InvalidConfig . show) (runReaderT app) errOrCfg


printResult :: ExceptT AppError IO [SchedulingResult] -> IO ()
printResult r = runExceptT r >>= putStrLn . formatAppRes
    where formatAppRes (Left (InvalidConfig msg)) = "ERROR - invalid config file: " ++ msg
          formatAppRes (Left e) = "ERROR: " ++ (show e)
          formatAppRes (Right x) = concat . intersperse "\n" $ map formatSchedRes x
          formatSchedRes (AlreadyScheduled d) = "already scheduled: " ++ (show d)
          formatSchedRes (EventScheduled e msg) = "event scheduled at: " ++ (show . eDate $ e) ++ ", cms msg: \"" ++ msg ++ "\""

