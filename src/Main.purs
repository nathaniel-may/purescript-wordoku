module Main where

import Prelude

import Data.Maybe (Maybe(..))
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
    , values :: Values
    , difficulty :: Difficulty
    , loading :: Boolean
    , generated :: Maybe String 
    }

data Action 
    = Generate Event
    | SetValues String

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

toValue :: String -> Values
toValue "Numbers" = Numbers
toValue "Colors" = Colors
toValue _ = Numbers

render st =
    HH.div_
        [ HH.form 
            [ HE.onSubmit (Just <<< Generate) ]
            [ HH.h1_ [ HH.text "Lookup GitHub user" ]
            , HH.label_
                [ HH.div_ [ HH.text "Enter username:" ]
                , HH.input
                    [ HP.value "value"
                    , HE.onValueInput (Just <<< SetValues)
                    ]
                ]
            , HH.button
                [ HP.disabled st.loading
                , HP.type_ HP.ButtonSubmit
                ]
                [ HH.text "Fetch info" ]
            , HH.p_
                [ HH.text (if st.loading then "Working..." else "") ]
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
    SetValues v -> do
        H.modify_ (_ { values = toValue v, generated = Nothing :: Maybe String })
    Generate event -> do
        H.liftEffect $ Event.preventDefault event
        st <- H.gets identity
        H.modify_ (_ { loading = true })
        sudoku <- H.liftEffect $ generate (fromState st)
        H.modify_ (_ { loading = false, generated = Just sudoku })
