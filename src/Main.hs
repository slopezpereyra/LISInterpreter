module Main where

import Test.HUnit
import Numeric.Natural

import Syntax 
import State
import Semantics
import TestSuite

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

