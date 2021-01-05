module Test.Main where

import Prelude

import Data.Array (drop, foldl)
import Data.NonEmpty ((:|))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Generator (Difficulty(..), Game(..), generate)
import Solver (CellSet(..), diagonalOf, readGrid)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Wordlist (wordlist)

main :: Effect Unit
main = void $ traverse quickCheck [test1]

newtype Word9 = Word9 String

runWord :: Word9 -> String
runWord (Word9 s) = s

instance arbWord9 :: Arbitrary Word9 where
  arbitrary = Word9 <$> (oneOf $ map pure ("abducting" :| drop 1 wordlist))

test1 :: Word9 -> Result
test1 word = unsafePerformEffect $ do
    let cellSet = CellSet '.' (toCharArray $ runWord word)
    str <- generate { difficulty: Beginner, restrictDiag: true, values: Wordoku}
    let solved = fromMaybe [] $ readGrid cellSet str
    let diag = (foldl (<>) "") <<< (map show) <<< diagonalOf $ solved
    let failureMsg = "Wordoku diagonal failed:\n" <> runWord word <> "\n" <> str <> "\n"
    pure $ (runWord word == diag) <?> failureMsg
    