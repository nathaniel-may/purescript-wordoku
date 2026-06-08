module Main where

import Prelude

import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (toUpper)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
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
import Sudoku (Difficulty(..), Game(..), Opts, Variant(..), emptySudoku, generateWithWorkers)
import Sudoku.Encoding (denormalize, normalize)
import Sudoku.Internal (chunksOf)
import Sudoku.Workers (WorkerPool, initPool, workerCount)
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
    { selected  :: { d :: Difficulty, g :: Game }
    , displayed :: Maybe { d :: Difficulty, g :: Game }
    , loading   :: Boolean
    , puzzle    :: String 
    , pool      :: Maybe WorkerPool
    }

data Action 
    = Initialize
    | PathChanged    String
    | Generate
    | NextGame       Game
    | NextDifficulty Difficulty

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
    }

fromState :: State -> Opts
fromState st = 
    { variant: if st.selected.g == Wordoku then UniqueDiagonal else Standard
    , values: st.selected.g
    , difficulty: st.selected.d
    }

cycle :: ∀ a. Enum a => a -> a -> a
cycle default = (fromMaybe default) <<< succ

tableFrom :: ∀ w i. Game -> String -> HH.HTML w i
tableFrom game s = case game of
    Colorku -> mkTable <<< colorkuRows $ s
    Wordoku -> mkTable <<< rows $ toUpper s
    Sudoku  -> mkTable <<< rows $ s
    where
        td' = HH.div [ HP.class_ (H.ClassName "td") ]

        rows :: String -> Array (Array (HH.HTML w i))
        rows str = chunksOf 9 $ (\v -> td' [ HH.text (displayChar v) ]) <$> (toCharArray str)
        
        colorkuRows :: String -> Array (Array (HH.HTML w i))
        colorkuRows str = chunksOf 9 $ (\color -> td' [ circle color ]) <$> (toCharArray str)

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
        circle _   = circle' "White"

        circle' :: String -> HH.HTML w i
        circle' color = HH.span [ HP.class_ (H.ClassName "Circle"), HP.attr (H.AttrName "Color") color ] []

        displayChar :: Char -> String
        displayChar '.'  = " "
        displayChar char = singleton char

        mkTable :: Array (Array (HH.HTML w i)) -> HH.HTML w i
        mkTable = HH.div [ HP.id "table" ] <<< map (HH.div [ HP.class_ $ H.ClassName "tr" ])

puzzleLabel :: ∀ w i. State -> HH.HTML w i
puzzleLabel st = HH.div_ [ HH.label [ HP.id "label" ] [ HH.text (label st.displayed) ] ] where
    
    label :: Maybe { d :: Difficulty, g :: Game } -> String
    label Nothing   = " "
    label (Just dg) = show dg.d <> " " <> show dg.g

render :: ∀ m. State -> H.ComponentHTML Action () m
render st =
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
                    , HP.name (show st.selected.d)
                    , HE.onClick (\_ -> NextDifficulty st.selected.d)
                    ]
                    [ HH.text (show st.selected.d) ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
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
                [ tableFrom (maybe Sudoku _.g st.displayed) st.puzzle 
                , puzzleLabel st
                ]
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
            Home -> H.modify_ (_ { puzzle = emptySudoku, displayed = Nothing })
            GameRoute g -> H.modify_ (_ { selected { g = g }, puzzle = emptySudoku, displayed = Nothing })
            DifficultyRoute g d -> H.modify_ (_ { selected { g = g, d = d }, puzzle = emptySudoku, displayed = Nothing })
            PuzzleRoute g d p k -> H.modify_ (_ { selected = { g, d }, displayed = Just { g, d }, puzzle = denormalize k p })

    NextGame g -> H.modify_ (_ { selected { g = cycle Sudoku g } })
    NextDifficulty d -> H.modify_ (_ { selected { d = cycle Beginner d } })
    Generate -> do
        st <- H.get
        case st.pool of
            Nothing -> H.liftEffect <<< log $ "Pool not initialized!"
            Just pool -> do
                H.liftEffect <<< log $ "generating a " <> show st.selected.d <> " " <> show st.selected.g <> "..."
                H.modify_ (_ { loading = true })
                start <- H.liftEffect $ map toDateTime now
                n      <- H.liftEffect workerCount
                result <- H.liftAff $ generateWithWorkers pool n (fromState st)
                end <- H.liftEffect $ map toDateTime now
                let { g, d } = st.selected
                H.modify_ (_ { displayed = Just { g, d }, loading = false, puzzle = result.puzzle })
                
                let normalizedPuzzle = normalize result.key result.puzzle
                    path = buildPath g d normalizedPuzzle result.key
                
                H.liftEffect do
                    h <- history =<< window
                    pushState (unsafeToForeign unit) (DocumentTitle "") (URL path) h
                    log $ "generated this game " <> show (diff end start :: Milliseconds) <> ":"
                    log result.puzzle

