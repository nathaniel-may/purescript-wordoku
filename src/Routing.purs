module Routing where

import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe(..))
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
    [gStr] -> 
      case parseGame gStr of
        Just g -> GameRoute g
        Nothing -> Home
    [gStr, dStr] -> 
      case parseGame gStr of
        Just g -> 
          case parseDifficulty dStr of
            Just d -> DifficultyRoute g d
            Nothing -> GameRoute g
        Nothing -> Home
    [gStr, dStr, pStr] -> 
      case parseGame gStr, parseDifficulty dStr of
        Just g, Just d -> 
          case decodePuzzle pStr of
            Just { puzzle, key } | isValidKey g key -> PuzzleRoute g d puzzle key
            _ -> DifficultyRoute g d
        Just g, Nothing -> GameRoute g
        _, _ -> Home
    _ -> Home

isValidKey :: Game -> String -> Boolean
isValidKey Sudoku k = k == "123456789"
isValidKey Colorku k = k == "ROYLGBIPV"
isValidKey Wordoku _ = true -- decodePuzzle already checked 9 distinct non-dot chars

buildPath :: Game -> Difficulty -> String -> String -> String
buildPath g d p k = "/" <> toLower (show g) <> "/" <> toLower (show d) <> "/" <> encodePuzzle p k
