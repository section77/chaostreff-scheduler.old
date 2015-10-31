module Main where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.List                  (concat, intersperse)
import           Scheduling
import           System.Environment         (getArgs, getProgName)
import           Types

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [u,p]     -> printResult $ scheduleEvents $ LoginData u p
    [u,p,y,m] -> printResult $ scheduleEventsAt (LoginData u p) (read y) (read m)
    _         -> putStrLn $ "usage: " ++ progName ++ " <USER> <PASS> [<YEAR> <MONTH>]"


printResult :: AppResult [SchedulingResult] -> IO ()
printResult r = runExceptT r >>= putStrLn . formatAppRes
    where formatAppRes (Left e) = "ERROR: " ++ (show e)
          formatAppRes (Right x) = concat . intersperse "\n" $ map formatSchedRes x
          formatSchedRes (AlreadyScheduled d) = "already scheduled: " ++ (show d)
          formatSchedRes (EventScheduled e msg) = "event scheduled at: " ++ (show . date $ e) ++ ", cms msg: \"" ++ msg ++ "\""

