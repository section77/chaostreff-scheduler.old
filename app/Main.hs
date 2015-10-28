module Main where

import           Data.List          (concat, intersperse)
import           Scheduling
import           System.Environment (getArgs, getProgName)
import           Types


main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [u,p]     -> print =<< scheduleEvents (LoginData u p)
    [u,p,y,m] -> print =<< scheduleEventsAt (LoginData u p) (read y) (read m)
    _         -> print $ "usage: " ++ progName ++ " <USER> <PASS> [<YEAR> <MONTH>]"
