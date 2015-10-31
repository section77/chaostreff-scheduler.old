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
    [u,p]     -> runExceptT (scheduleEvents $ LoginData u p) >>= print
    [u,p,y,m] -> runExceptT (scheduleEventsAt (LoginData u p) (read y) (read m)) >>= print
    _         -> print $ "usage: " ++ progName ++ " <USER> <PASS> [<YEAR> <MONTH>]"
