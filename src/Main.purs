module Main where

import Prelude

import Data.Array ((..))
import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Generator (Difficulty(..), Game(..), Opts, generate)
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
    { restrictDiag :: Boolean
    , game         :: Game
    , difficulty   :: Difficulty
    , loading      :: Boolean
    , generated    :: Maybe String 
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
    , game: Wordoku
    , difficulty: Tricky
    , loading: false
    , generated: Nothing 
    }

fromState :: State -> Opts
fromState st = 
    { restrictDiag: (st.game == Wordoku)
    , values: st.game
    , difficulty: st.difficulty 
    }

cycle :: ∀ a. Enum a => a -> a -> a
cycle default = (fromMaybe default) <<< succ

table :: ∀ a b. HTML a b
table = HH.table_ $ (HH.tr [ HP.class_ (H.ClassName "Row") ]) <$> rows where 
    rows :: ∀ a' b'. Array (Array (HTML a' b'))
    rows = chunksOf 9 $ (\n -> HH.td [ HP.id_ (show n) ] [ HH.text "X" ]) <$> (0..80)

render :: ∀ a. State -> HTML a Action
render st =
    HH.div 
        [ HP.class_ (H.ClassName "VContainer") ]
        [ HH.div_
            [ HH.h1_ [ HH.text "Sudoku Generator" ]
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
                    , HE.onClick (\_ -> Just $ NextGame st.game)
                    ]
                    [ HH.text (show st.game) ]
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
            , HH.div [ HP.class_ (H.ClassName "VContainer") ] [ table ]
            , HH.div_
                case st.generated of
                    Nothing -> []
                    Just res ->
                        [ HH.h2_
                            [ HH.text "Response:" ]
                        , HH.pre_
                            [ HH.code_ [ HH.text res ] ]
                        ]
            ]
        ]

handleAction :: ∀ o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
    NextGame g -> do
        H.liftEffect <<< log $ "game changed to " <> show g
        H.modify_ (_ { game = cycle Sudoku g })
    NextDifficulty d -> do
        H.liftEffect <<< log $ "difficulty changed to " <> show d
        H.modify_ (_ { difficulty = cycle Beginner d })
    Generate -> do
        H.liftEffect $ log "generating..."
        st <- H.gets identity
        H.modify_ (_ { loading = true })
        sudoku <- H.liftAff <<< H.liftEffect $ generate (fromState st)
        H.liftEffect $ log "generated this game:"
        H.liftEffect $ log sudoku
        H.modify_ (_ { loading = false, generated = Just sudoku })
