module Routing where

import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, toLower)
import Sudoku.Encoding (decodePuzzle, encodePuzzle)
import Sudoku.Internal.Generator (Difficulty(..), Game(..))

data Route
  = Home
  | GameRoute Game
  | DifficultyRoute Game Difficulty
  | PuzzleRoute Game Difficulty String String

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show Home = "Home"
  show (GameRoute g) = "GameRoute " <> show g
  show (DifficultyRoute g d) = "DifficultyRoute " <> show g <> " " <> show d
  show (PuzzleRoute g d p k) = "PuzzleRoute " <> show g <> " " <> show d <> " " <> p <> " " <> k

parseGame :: String -> Maybe Game
parseGame s = case toLower s of
  "sudoku" -> Just Sudoku
  "wordoku" -> Just Wordoku
  "colorku" -> Just Colorku
  _ -> Nothing

parseDifficulty :: String -> Maybe Difficulty
parseDifficulty s = case toLower s of
  "beginner" -> Just Beginner
  "casual" -> Just Casual
  "tricky" -> Just Tricky
  "difficult" -> Just Difficult
  "challenge" -> Just Challenge
  _ -> Nothing

parsePath :: String -> Route
parsePath path = 
  let segments = filter (_ /= "") (split (Pattern "/") path)
  in case segments of
    [] -> Home
    [gStr] -> fromMaybe Home $ map GameRoute (parseGame gStr)
    [gStr, dStr] -> fromMaybe (parsePath gStr) do
      g <- parseGame gStr
      pure $ fromMaybe (GameRoute g) $ map (DifficultyRoute g) (parseDifficulty dStr)
    [gStr, dStr, pStr] -> fromMaybe (parsePath ("/" <> gStr <> "/" <> dStr)) do
      g <- parseGame gStr
      d <- parseDifficulty dStr
      { puzzle, key } <- decodePuzzle pStr
      pure $ PuzzleRoute g d puzzle key
    _ -> Home

buildPath :: Game -> Difficulty -> String -> String -> String
buildPath g d p k = "/" <> toLower (show g) <> "/" <> toLower (show d) <> "/" <> encodePuzzle p k
