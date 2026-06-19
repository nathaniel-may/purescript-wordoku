module Test.Full (main) where

import Prelude

import Data.Traversable (traverse)
import Effect (Effect)
import Test.Main (runFastTests)
import Test.SlowGeneratorTests (slowGeneratorTests)
import Test.QuickCheck (quickCheck')

main :: Effect Unit
main = do
  runFastTests
  slowResults <- slowGeneratorTests
  void $ traverse (quickCheck' 1) slowResults
