{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Test.DocTest
import           Protolude


main :: IO ()
main = doctest [
    "-packageghc"
  , "-isrc"
  , "src"
  ]
