module Main where

import           Args
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.List                  (concat, intersperse)
import           Data.Version               (showVersion)
import           Data.Yaml
import           Paths_chaostreff_scheduler (version)
import           Scheduling
import           System.Environment         (getArgs)
import           Text.Parsec                (ParseError)
import           Types


main :: IO ()
main = do
  args <- parseArgs . unwords <$> getArgs
  either handleInvalidArgs run args


run :: AppArgs -> IO ()
run ShowHelp = printUsage
run ShowVersion = putStrLn $ showVersion version
run (ScheduleNextMonths cfgFile n) = printResult $ runApp (scheduleNextEvents n) cfgFile
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



printUsage :: IO ()
printUsage = putStrLn $ unlines [
               "chaostreff-scheduler - schedule chaostreff events"
             , ""
             , "usage:"
             , "  chaostreff-scheduler <CFG> [<YEAR> <MONTH> | <COUNT>]"
             , ""
             , "examples:"
             , " chaostreff-scheduler chaostreff-scheduler.yaml"
             , "   -> schedule for the actual year and month"
             , ""
             , " chaostreff-scheduler chaostreff-scheduler.yaml 2"
             , "   -> schedule for the actual and next month"
             , ""
             , " chaostreff-scheduler chaostreff-scheduler.yaml 2016 01"
             , "   -> schedule for january 2016"
             , ""
             , "other flags:"
             , " -v: print version"
             , " -h: show this help"
             ]


handleInvalidArgs :: ParseError -> IO ()
handleInvalidArgs e = do
  putStrLn $ show e
  putStrLn ""
  printUsage
