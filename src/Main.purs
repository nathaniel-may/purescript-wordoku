module Main where

import Prelude

import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Generator (Difficulty(..), Game(..), Opts, emptySudoku, generate)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Lib (chunksOf)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
    { restrictDiag  :: Boolean
    , selectedGame  :: Game
    , displayedGame :: Game
    , difficulty    :: Difficulty
    , loading       :: Boolean
    , puzzle        :: String 
    }

data Action 
    = Generate
    | NextGame       Game
    | NextDifficulty Difficulty

component :: ∀ m a b c. MonadAff m => Component HTML a b c m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: ∀ i. i -> State
initialState _ = 
    { restrictDiag: false
    , selectedGame: Wordoku
    , displayedGame: Wordoku
    , difficulty: Tricky
    , loading: false
    , puzzle: emptySudoku 
    }

fromState :: State -> Opts
fromState st = 
    { restrictDiag: (st.selectedGame == Wordoku)
    , values: st.selectedGame
    , difficulty: st.difficulty 
    }

cycle :: ∀ a. Enum a => a -> a -> a
cycle default = (fromMaybe default) <<< succ

tableFrom :: ∀ a b. Game -> String -> HTML a b
tableFrom game s = case game of
    Colorku -> mkTable <<< colorkuRows $ s
    Wordoku -> mkTable <<< rows $ toUpper s
    Sudoku  -> mkTable <<< rows $ s
    where
        rows :: String -> Array (Array (HTML a b))
        rows str = chunksOf 9 $ (\v -> HH.td_ [ HH.text (displayChar v) ]) <$> (toCharArray str)
        
        colorkuRows :: String -> Array (Array (HTML a b))
        colorkuRows str = chunksOf 9 $ (\color -> HH.td_ [ circle color ]) <$> (toCharArray str)

        circle :: Char -> HTML a b
        circle 'R' = circle' "Red"
        circle 'O' = circle' "Orange"
        circle 'Y' = circle' "Yellow"
        circle 'L' = circle' "Lime"
        circle 'G' = circle' "Green"
        circle 'B' = circle' "Blue"
        circle 'I' = circle' "Indigo"
        circle 'P' = circle' "Purple"
        circle 'V' = circle' "Violet"
        circle _   = HH.text " "

        circle' :: String -> HTML a b
        circle' color = HH.span [ HP.class_ (H.ClassName "Circle"), HP.attr (H.AttrName "Color") color ] []

        displayChar :: Char -> String
        displayChar '.'  = " "
        displayChar char = singleton char

        mkTable :: Array (Array (HTML a b)) -> HTML a b
        mkTable = HH.table_ <<< map (HH.tr [ HP.class_ (H.ClassName "Row") ])

render :: ∀ a. State -> HTML a Action
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
                    , HP.name (show st.difficulty)
                    , HE.onClick (\_ -> Just $ NextDifficulty st.difficulty)
                    ]
                    [ HH.text (show st.difficulty) ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HE.onClick (\_ -> Just $ NextGame st.selectedGame)
                    ]
                    [ HH.text (show st.selectedGame) ]
                ]
            , HH.div
                [ HP.class_ (H.ClassName "VContainer") ] 
                [ HH.div_ [ HH.text "" ]
                , HH.button
                    [ HP.disabled st.loading
                    , HP.name "Generate"
                    , HP.type_ HP.ButtonButton
                    , HE.onClick (\_ -> Just Generate)
                    ]
                    [ HH.text (if st.loading then "Working..." else "Generate") ]
                ]
            , HH.div 
                [ HP.class_ (H.ClassName "VContainer") ] 
                [ tableFrom st.displayedGame st.puzzle ]
            ]
        ]

handleAction :: ∀ o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
    NextGame g -> do
        H.liftEffect <<< log $ "game changed to " <> show g
        H.modify_ (_ { selectedGame = cycle Sudoku g })
    NextDifficulty d -> do
        H.liftEffect <<< log $ "difficulty changed to " <> show d
        H.modify_ (_ { difficulty = cycle Beginner d })
    Generate -> do
        H.liftEffect $ log "generating..."
        st <- H.gets identity
        H.modify_ (_ { displayedGame = st.selectedGame, loading = true })
        sudoku <- H.liftAff <<< H.liftEffect $ generate (fromState st)
        H.liftEffect $ log "generated this game:"
        H.liftEffect $ log sudoku
        H.modify_ (_ { loading = false, puzzle = sudoku })
