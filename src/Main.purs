module Main where

import Prelude

import Data.Enum (class Enum, succ)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Generator (Values(..), Difficulty(..), Opts, generate)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
    { restrictDiag :: Boolean
    , values       :: Values
    , difficulty   :: Difficulty
    , loading      :: Boolean
    , generated    :: Maybe String 
    }

data Action 
    = Generate      Event
    | NextValues     Values
    | NextDifficulty Difficulty

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = 
    { restrictDiag: false
    , values: Numbers
    , difficulty: Difficult
    , loading: false
    , generated: Nothing 
    }

fromState :: State -> Opts
fromState st = { restrictDiag: false, values: st.values, difficulty: st.difficulty }

cycle :: ∀ a. Enum a => a -> a -> a
cycle default = (fromMaybe default) <<< succ

render st =
    HH.div_
        [ HH.form 
            [ HE.onSubmit (Just <<< Generate) ]
            [ HH.h1_ [ HH.text "Sudoku Generator" ]
        , HH.div_
            [ HH.div_
                [ HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.name (show st.difficulty)
                    , HE.onClick (\_ -> Just $ NextDifficulty st.difficulty)
                    ]
                    [ HH.text (show st.difficulty) ]
                ]
            , HH.button
                [ HP.type_ HP.ButtonButton
                , HE.onClick (\_ -> Just $ NextValues st.values)
                ]
                [ HH.text (show st.values) ]
            ]
            , HH.label_ 
                [ HH.div_ [ HH.text "" ]
                , HH.button
                    [ HP.disabled st.loading
                    , HP.type_ HP.ButtonSubmit
                    ]
                    [ HH.text (if st.loading then "Working..." else "Generate") ]
                ]
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

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
    NextValues v -> do
        H.modify_ (_ { values = cycle Numbers v })
    NextDifficulty d -> do
        H.modify_ (_ { difficulty = cycle Beginner d })
    Generate event -> do
        H.liftEffect $ Event.preventDefault event
        st <- H.gets identity
        H.modify_ (_ { loading = true })
        sudoku <- H.liftEffect $ generate (fromState st)
        H.modify_ (_ { loading = false, generated = Just sudoku })
