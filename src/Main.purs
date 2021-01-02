module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Generator (Values(..), Difficulty(..), generate)

main :: Effect Unit
main = log =<< generate { restrictDiag: false, values: Numbers, difficulty: Difficult }
