module Test.Main where

import Prelude

import Data.Array (elem, foldl)
import Data.Either (Either(..), hush)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Generator (Difficulty(..), Game(..), generate)
import Solver (cellSetFromPuzzle, diagonalOf, readGrid, solve)
import Test.QuickCheck (class Testable, Result, quickCheck, (<?>))
import Wordlist (wordlist)

main :: Effect Unit
main = void $ traverse quickCheckAff [test1]

quickCheckAff :: forall prop. Testable prop => Aff prop -> Effect Unit
quickCheckAff = runAff_ (\e -> case e of 
    Left _ -> pure unit
    Right result -> quickCheck result)

test1 :: Aff Result
test1 = do
    str <- generate { difficulty: Beginner, restrictDiag: true, values: Wordoku }
    -- solved relies on all 9 characters being present in the generated puzzle. (not given)
    let solved = fromMaybe [] $ (solve true) =<< (flip readGrid str) =<< (hush $ cellSetFromPuzzle str)
    let diag = (foldl (<>) "") <<< (map show) <<< diagonalOf $ solved
    let failureMsg = "Wordoku diagonal failed:\n" <> diag <> "\n" <> str <> "\n"
    pure $ (diag `elem` wordlist) <?> failureMsg
    