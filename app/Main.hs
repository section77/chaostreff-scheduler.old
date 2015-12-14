module Main where

import           Args
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.List                  (concat, intersperse)
import           Options.Applicative        (execParser, fullDesc, header,
                                             helper, info, progDesc, (<>))
import           Scheduling
import           Types


main :: IO ()
main = execParser opts >>= run
    where opts = info (helper <*> appArgs)
                 ( fullDesc
                 <> progDesc "schedule chaostreff events"
                 <> header "chaostreff scheduler")


run :: AppArgs -> IO ()
run (ScheduleThisMonth u p) = printResult $ scheduleEvents $ LoginData u p
run (ScheduleMonth u p y m) = printResult $ scheduleEventsAt (LoginData u p) y m


printResult :: AppResult [SchedulingResult] -> IO ()
printResult r = runExceptT r >>= putStrLn . formatAppRes
    where formatAppRes (Left e) = "ERROR: " ++ (show e)
          formatAppRes (Right x) = concat . intersperse "\n" $ map formatSchedRes x
          formatSchedRes (AlreadyScheduled d) = "already scheduled: " ++ (show d)
          formatSchedRes (EventScheduled e msg) = "event scheduled at: " ++ (show . date $ e) ++ ", cms msg: \"" ++ msg ++ "\""

