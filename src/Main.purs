module Main where

import Prelude

import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (toUpper)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, effectCanceler, makeAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Now (now)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Sudoku (Difficulty(..), Game(..), Opts, Variant(..), emptySudoku, generate)
import Sudoku.Internal (chunksOf)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
    { selected  :: { d :: Difficulty, g :: Game }
    , displayed :: Maybe { d :: Difficulty, g :: Game }
    , loading   :: Boolean
    , puzzle    :: String 
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
    { selected: { d: Tricky, g: Wordoku }
    , displayed: Nothing
    , loading: false
    , puzzle: emptySudoku 
    }

fromState :: State -> Opts
fromState st = 
    { variant: if st.selected.g == Wordoku then UniqueDiagonal else Standard
    , values: st.selected.g
    , difficulty: st.selected.d
    }

cycle :: ∀ a. Enum a => a -> a -> a
cycle default = (fromMaybe default) <<< succ

tableFrom :: ∀ a b. Game -> String -> HTML a b
tableFrom game s = case game of
    Colorku -> mkTable <<< colorkuRows $ s
    Wordoku -> mkTable <<< rows $ toUpper s
    Sudoku  -> mkTable <<< rows $ s
    where
        td' = HH.div [ HP.class_ (H.ClassName "td") ]

        rows :: String -> Array (Array (HTML a b))
        rows str = chunksOf 9 $ (\v -> td' [ HH.text (displayChar v) ]) <$> (toCharArray str)
        
        colorkuRows :: String -> Array (Array (HTML a b))
        colorkuRows str = chunksOf 9 $ (\color -> td' [ circle color ]) <$> (toCharArray str)

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
        circle _   = circle' "White"

        circle' :: String -> HTML a b
        circle' color = HH.span [ HP.class_ (H.ClassName "Circle"), HP.attr (H.AttrName "Color") color ] []

        displayChar :: Char -> String
        displayChar '.'  = " "
        displayChar char = singleton char

        mkTable :: Array (Array (HTML a b)) -> HTML a b
        mkTable = HH.div [ HP.id_ "table" ] <<< map (HH.div [ HP.class_ $ H.ClassName "tr" ])

puzzleLabel :: ∀ a b. State -> HTML a b
puzzleLabel st = HH.div_ [ HH.label [ HP.id_ "label" ] [ HH.text (label st.displayed) ] ] where
    
    label :: Maybe { d :: Difficulty, g :: Game } -> String
    label Nothing   = " "
    label (Just dg) = show dg.d <> " " <> show dg.g

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
                    , HP.name (show st.selected.d)
                    , HE.onClick (\_ -> Just $ NextDifficulty st.selected.d)
                    ]
                    [ HH.text (show st.selected.d) ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HE.onClick (\_ -> Just $ NextGame st.selected.g)
                    ]
                    [ HH.text (show st.selected.g) ]
                ]
            , HH.div
                [ HP.class_ (H.ClassName "VContainer") ] 
                [ HH.button
                    [ HP.disabled st.loading
                    , HP.id_ "Generate"
                    , HP.type_ HP.ButtonButton
                    , HE.onClick (\_ -> Just Generate)
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
    NextGame g -> H.modify_ (_ { selected { g = cycle Sudoku g } })
    NextDifficulty d -> H.modify_ (_ { selected { d = cycle Beginner d } })
    Generate -> do
        st <- H.get
        H.liftEffect <<< log $ "generating a " <> show st.selected.d <> " " <> show st.selected.g <> "..."
        H.modify_ (_ { loading = true })
        start <- H.liftEffect $ map toDateTime now
        sudoku <- H.liftAff $ (delay $ Milliseconds 4.0) *> makeAff (\cb -> do
            val <- generate $ fromState st
            _   <- cb (Right val)
            pure <<< effectCanceler $ log "generation canceled")
        end <- H.liftEffect $ map toDateTime now
        H.modify_ (_ { displayed = Just st.selected, loading = false, puzzle = sudoku })
        H.liftEffect <<< log $ "generated this game " <> show (diff end start :: Milliseconds) <> ":"
        H.liftEffect $ log sudoku

