module Main where

import Simple
import Sum
import Value

import Test.Hspec


main :: IO ()
main = hspec $ do
  simpleSpec
  sumSpec
  valueSpec
