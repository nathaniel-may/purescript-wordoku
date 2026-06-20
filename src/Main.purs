module Main where

import Prelude

import Data.Array as Array
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (toUpper)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (try)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Now (now)
import Foreign (unsafeToForeign)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Routing (Route(..), buildPath, parsePath)
import Sudoku (Difficulty(..), Game(..), Grid, Opts, Variant(..), emptySudoku, generateWithWorkers, mkKey, readGrid)
import Sudoku.Display (addClueButtonDisabled, applyClues, applySolveResult, displayedPuzzleString, solutionButtonDisabled, solutionButtonLabel)
import Sudoku.Encoding (DecodedKey, denormalize, keyToString, normalize)
import Sudoku.Internal (chunksOf)
import Sudoku.Workers (WorkerPool, initPool, solvePuzzle, workerCount)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), pushState)
import Web.HTML.Location (pathname)
import Web.HTML.Window (history, location, toEventTarget)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
  { selected :: { d :: Difficulty, g :: Game }
  , displayed :: Maybe { d :: Difficulty, g :: Game }
  , loading :: Boolean
  , puzzle :: String
  , pool :: Maybe WorkerPool
  , decodedKey :: Maybe DecodedKey
  , solution :: Maybe Grid
  , solveError :: Maybe String
  , showingSolution :: Boolean
  , solveRequestId :: Int
  , clueCount :: Int
  }

data Action
  = Initialize
  | PathChanged String
  | Generate
  | NextGame Game
  | NextDifficulty Difficulty
  | RequestSolve Int DecodedKey String
  | SolveFinished Int DecodedKey (Either String String)
  | ToggleSolution
  | AddClue

component :: ∀ q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: ∀ i. i -> State
initialState _ =
  { selected: { d: Tricky, g: Wordoku }
  , displayed: Nothing
  , loading: false
  , puzzle: emptySudoku
  , pool: Nothing
  , decodedKey: Nothing
  , solution: Nothing
  , solveError: Nothing
  , showingSolution: false
  , solveRequestId: 0
  , clueCount: 0
  }

fromState :: State -> Opts
fromState st =
  { variant: if st.selected.g == Wordoku then UniqueDiagonal else Standard
  , values: st.selected.g
  , difficulty: st.selected.d
  }

cycle :: ∀ a. Enum a => a -> a -> a
cycle default = (fromMaybe default) <<< succ

tableFrom :: ∀ w i. Game -> String -> String -> HH.HTML w i
tableFrom game original displayed = case game of
  Colorku -> mkTable <<< colorkuRows $ Array.zip (toCharArray original) (toCharArray displayed)
  Wordoku -> mkTable <<< rows $ Array.zip (toCharArray original) (toCharArray (toUpper displayed))
  Sudoku -> mkTable <<< rows $ Array.zip (toCharArray original) (toCharArray displayed)
  where
  td' :: Boolean -> Array (HH.HTML w i) -> HH.HTML w i
  td' isExtraClue =
    HH.div [ HP.classes ([ H.ClassName "td" ] <> if isExtraClue then [ H.ClassName "ExtraClue" ] else []) ]

  isExtra :: Char -> Char -> Boolean
  isExtra origChar dispChar = origChar == '.' && dispChar /= '.'

  rows :: Array (Tuple Char Char) -> Array (Array (HH.HTML w i))
  rows pairs = chunksOf 9 $ (\(Tuple o d) -> td' (isExtra o d) [ HH.text (displayChar d) ]) <$> pairs

  colorkuRows :: Array (Tuple Char Char) -> Array (Array (HH.HTML w i))
  colorkuRows pairs = chunksOf 9 $ (\(Tuple o d) -> td' (isExtra o d) [ circle d ]) <$> pairs

  circle :: Char -> HH.HTML w i
  circle 'R' = circle' "Red"
  circle 'O' = circle' "Orange"
  circle 'Y' = circle' "Yellow"
  circle 'L' = circle' "Lime"
  circle 'G' = circle' "Green"
  circle 'B' = circle' "Blue"
  circle 'I' = circle' "Indigo"
  circle 'P' = circle' "Purple"
  circle 'V' = circle' "Violet"
  circle _ = circle' "White"

  circle' :: String -> HH.HTML w i
  circle' color = HH.span [ HP.class_ (H.ClassName "Circle"), HP.attr (H.AttrName "Color") color ] []

  displayChar :: Char -> String
  displayChar '.' = " "
  displayChar char = singleton char

  mkTable :: Array (Array (HH.HTML w i)) -> HH.HTML w i
  mkTable = HH.div [ HP.id "table" ] <<< map (HH.div [ HP.class_ $ H.ClassName "tr" ])

puzzleLabel :: ∀ w i. State -> HH.HTML w i
puzzleLabel st = HH.div_ [ HH.label [ HP.id "label" ] [ HH.text (label st.displayed) ] ]
  where

  label :: Maybe { d :: Difficulty, g :: Game } -> String
  label Nothing = " "
  label (Just dg) = show dg.d <> " " <> show dg.g

currentPuzzleDisplay :: State -> { original :: String, displayed :: String }
currentPuzzleDisplay st = case st.decodedKey of
  Nothing -> { original: st.puzzle, displayed: st.puzzle }
  Just key
    | st.showingSolution ->
        let
          s = displayedPuzzleString key true st.puzzle st.solution
        in
          { original: s, displayed: s }
    | otherwise ->
        { original: st.puzzle
        , displayed: fromMaybe st.puzzle (applyClues key st.clueCount st.puzzle <$> st.solution)
        }

render :: ∀ m. State -> H.ComponentHTML Action () m
render st =
  let
    display = currentPuzzleDisplay st
  in
    HH.div
      [ HP.class_ (H.ClassName "VContainer") ]
      [ HH.div_
          [ HH.div
              [ HP.class_ (H.ClassName "HContainer") ]
              [ HH.h1_ [ HH.text "Sudoku Generator" ] ]
          , HH.div
              [ HP.class_ (H.ClassName "HContainer") ]
              [ HH.button
                  [ HP.type_ HP.ButtonButton
                  , HP.id "Difficulty"
                  , HP.name (show st.selected.d)
                  , HE.onClick (\_ -> NextDifficulty st.selected.d)
                  ]
                  [ HH.text (show st.selected.d) ]
              , HH.button
                  [ HP.type_ HP.ButtonButton
                  , HP.id "Game"
                  , HE.onClick (\_ -> NextGame st.selected.g)
                  ]
                  [ HH.text (show st.selected.g) ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "VContainer") ]
              [ HH.button
                  [ HP.disabled st.loading
                  , HP.id "Generate"
                  , HP.type_ HP.ButtonButton
                  , HE.onClick (\_ -> Generate)
                  ]
                  [ HH.text if st.loading then "Working..." else "Generate" ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "label") ]
              [ tableFrom (maybe Sudoku _.g st.displayed) display.original display.displayed
              , puzzleLabel st
              ]
          , HH.div
              [ HP.class_ (H.ClassName "VContainer") ]
              ( ( case st.solveError of
                    Just err ->
                      [ HH.div
                          [ HP.class_ (H.ClassName "SolveError") ]
                          [ HH.text ("Error solving puzzle: " <> err) ]
                      ]
                    Nothing -> []
                )
                  <>
                    [ HH.div
                        [ HP.class_ (H.ClassName "ButtonSlot")
                        , HP.style if isJust st.displayed then "" else "visibility: hidden"
                        ]
                        [ HH.button
                            [ HP.disabled (solutionButtonDisabled st.solution)
                            , HP.id "Solution"
                            , HP.type_ HP.ButtonButton
                            , HE.onClick (\_ -> ToggleSolution)
                            ]
                            [ HH.text (solutionButtonLabel st.showingSolution) ]
                        , HH.button
                            [ HP.disabled (solutionButtonDisabled st.solution || addClueButtonDisabled st.solution st.clueCount st.puzzle)
                            , HP.id "AddClue"
                            , HP.type_ HP.ButtonButton
                            , HE.onClick (\_ -> AddClue)
                            ]
                            [ HH.text "+1 Clue" ]
                        ]
                    ]
              )
          , HH.div
              [ HP.class_ (H.ClassName "VContainer") ]
              [ HH.footer_
                  [ HH.text "PureScript + Netlify | Source on "
                  , HH.a
                      [ HP.href "https://github.com/nathaniel-may/purescript-wordoku" ]
                      [ HH.text "GitHub" ]
                  ]
              ]
          ]
      ]

handleAction :: ∀ o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    pool <- H.liftEffect initPool
    H.modify_ (_ { pool = Just pool })
    { emitter, listener } <- H.liftEffect HS.create
    void $ H.subscribe emitter
    H.liftEffect do
      w <- window
      let et = toEventTarget w
      cb <- eventListener \_ -> do
        loc <- location w
        path <- pathname loc
        HS.notify listener (PathChanged path)
      addEventListener (EventType "popstate") cb false et

      loc <- location w
      path <- pathname loc
      HS.notify listener (PathChanged path)

  PathChanged path -> do
    let route = parsePath path
    case route of
      Home -> H.modify_
        ( _
            { puzzle = emptySudoku
            , displayed = Nothing
            , decodedKey = Nothing
            , solution = Nothing
            , solveError = Nothing
            , showingSolution = false
            , clueCount = 0
            }
        )
      GameRoute g -> H.modify_
        ( _
            { selected { g = g }
            , puzzle = emptySudoku
            , displayed = Nothing
            , decodedKey = Nothing
            , solution = Nothing
            , solveError = Nothing
            , showingSolution = false
            , clueCount = 0
            }
        )
      DifficultyRoute g d -> H.modify_
        ( _
            { selected { g = g, d = d }
            , puzzle = emptySudoku
            , displayed = Nothing
            , decodedKey = Nothing
            , solution = Nothing
            , solveError = Nothing
            , showingSolution = false
            , clueCount = 0
            }
        )
      PuzzleRoute g d p k n -> do
        let
          displayPuzzle = denormalize k p
          blankCount = Array.length (Array.filter (_ == '.') (toCharArray displayPuzzle))
          clampedClueCount = max 0 (min n blankCount)
        H.modify_
          ( _
              { selected = { g, d }
              , displayed = Just { g, d }
              , puzzle = displayPuzzle
              , clueCount = clampedClueCount
              , showingSolution = false
              , solution = Nothing
              , solveError = Nothing
              }
          )
        dispatchSolve k displayPuzzle

  NextGame g -> H.modify_ (_ { selected { g = cycle Sudoku g } })
  NextDifficulty d -> H.modify_ (_ { selected { d = cycle Beginner d } })
  Generate -> do
    st <- H.get
    case st.pool of
      Nothing -> H.liftEffect <<< log $ "Pool not initialized!"
      Just pool -> do
        H.modify_ (_ { loading = true })
        start <- H.liftEffect $ map toDateTime now
        n <- H.liftEffect workerCount
        H.liftEffect <<< log $ "generating a " <> show st.selected.d <> " " <> show st.selected.g <> " with " <> show n <> " workers..."
        result <- H.liftAff $ generateWithWorkers pool n (fromState st)
        end <- H.liftEffect $ map toDateTime now
        let { g, d } = st.selected
        H.modify_
          ( _
              { displayed = Just { g, d }
              , loading = false
              , puzzle = result.puzzle
              , clueCount = 0
              , showingSolution = false
              , solution = Nothing
              , solveError = Nothing
              }
          )

        let
          normalizedPuzzle = normalize result.key result.puzzle
          path = buildPath g d normalizedPuzzle result.key 0

        H.liftEffect do
          h <- history =<< window
          pushState (unsafeToForeign unit) (DocumentTitle "") (URL path) h
          log $ "generated this game " <> show (diff end start :: Milliseconds) <> ":"
          log result.puzzle

        dispatchSolve result.key result.puzzle

  RequestSolve reqId key displayPuzzle -> do
    st <- H.get
    case st.pool of
      Nothing -> H.liftEffect <<< log $ "Pool not initialized! Cannot solve."
      Just pool -> do
        result <- H.liftAff do
          attempt <- try (solvePuzzle pool (keyToString key) displayPuzzle)
          pure $ case attempt of
            Right solvedStr -> Right solvedStr
            Left err -> Left (message err)
        handleAction (SolveFinished reqId key result)

  SolveFinished reqId key result -> do
    st <- H.get
    let
      asGrid :: Either String String -> Either String Grid
      asGrid (Left err) = Left err
      asGrid (Right s) = do
        k <- mkKey (keyToString key)
        readGrid k s

      applied = applySolveResult
        { latestRequestId: st.solveRequestId
        , currentSolution: st.solution
        , currentSolveError: st.solveError
        }
        reqId
        (asGrid result)

    when (reqId /= st.solveRequestId) do
      H.liftEffect <<< log $ "Ignoring stale solve result for request " <> show reqId
    case applied.solveError of
      Just err | reqId == st.solveRequestId ->
        H.liftEffect <<< log $ "BUG: solve failed for a displayed puzzle: " <> err
      _ -> pure unit

    H.modify_ (_ { solution = applied.solution, solveError = applied.solveError })

  ToggleSolution -> do
    st <- H.get
    case st.solution of
      Nothing -> pure unit -- no-op: nothing to toggle to (button is disabled in this state)
      Just _ -> H.modify_ (_ { showingSolution = not st.showingSolution })

  AddClue -> do
    st <- H.get
    case st.decodedKey of
      Nothing -> pure unit -- no-op: nothing to reveal a clue for
      Just key -> do
        let
          blankCount = Array.length (Array.filter (_ == '.') (toCharArray st.puzzle))
          newCount = min (st.clueCount + 1) blankCount
        H.modify_ (_ { clueCount = newCount })
        let
          normalizedPuzzle = normalize key st.puzzle
          path = buildPath st.selected.g st.selected.d normalizedPuzzle key newCount
        H.liftEffect do
          h <- history =<< window
          pushState (unsafeToForeign unit) (DocumentTitle "") (URL path) h

-- | Resets solve-related state for a newly-displayed puzzle, bumps the
-- | request id, and dispatches a background solve. Used by both `Generate`'s
-- | success branch and `PathChanged`'s `PuzzleRoute` branch.
dispatchSolve :: ∀ o m. MonadAff m => DecodedKey -> String -> H.HalogenM State Action () o m Unit
dispatchSolve key displayPuzzle = do
  st <- H.get
  let reqId = st.solveRequestId + 1
  H.modify_
    ( _
        { decodedKey = Just key
        , solution = Nothing
        , solveError = Nothing
        , showingSolution = false
        , solveRequestId = reqId
        }
    )
  handleAction (RequestSolve reqId key displayPuzzle)
