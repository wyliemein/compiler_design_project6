{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common

import Test.Tasty
import Paths_fox

main :: IO ()
main = do
  anfTestsFile     <- getDataFileName "tests/anf.json"
  adderTestsFile   <- getDataFileName "tests/adder.json"
  boaTestsFile     <- getDataFileName "tests/boa.json"
  cobraTestsFile   <- getDataFileName "tests/cobra.json"
  diamondTestsFile <- getDataFileName "tests/diamondback.json"
  eggTestsFile     <- getDataFileName "tests/egg.json"
  foxTestsFile     <- getDataFileName "tests/fox.json"
  yourTestsFile    <- getDataFileName "tests/yourTests.json"

  anfTests     <- readTests anfTestsFile
  adderTests   <- readTests adderTestsFile
  boaTests     <- readTests boaTestsFile
  cobraTests   <- readTests cobraTestsFile
  diamondTests <- readTests diamondTestsFile
  eggTests     <- readTests eggTestsFile
  foxTests     <- readTests foxTestsFile
  yourTests    <- readTests yourTestsFile
 
  let tests = testGroup "Tests" $
                -- [ testGroup "Normalizer"      anfTests
                -- , testGroup "Adder"           adderTests
                -- , testGroup "Boa"             boaTests
                -- , testGroup "Cobra"           cobraTests
                -- , testGroup "Diamondback"     diamondTests
                -- , testGroup "Egg-Eater"       eggTests 
                [
                 testGroup "Fox"             foxTests
                , testGroup "Your-Tests"      yourTests
                ]
  defaultMain tests

readTests :: FilePath -> IO [TestTree]
readTests f = map createTestTree <$> parseTestFile f
