module Editor where

import Prelude hiding (div)
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Web.Event.Event (Event)
import Web.Event.Event as WE
import Halogen as H
import Halogen (mkComponent, mkEval, defaultEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import UntypedLambda (Environment, eval, display, standardLibs)

type State
  = { input :: String
    , history :: Array { input :: String, output :: String }
    , env :: Environment
    }

data Action
  = PreventDefault Event Action
  | Input String
  | Eval

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI
      ( mkComponent
          { initialState
          , render
          , eval: mkEval $ defaultEval { handleAction = handleAction }
          }
      )
      unit
      body
  where
  initialState :: Unit -> State
  initialState _ =
    { input: ""
    , history: []
    , env: []
    }

  handleAction = case _ of
    PreventDefault event action -> do
      H.liftEffect $ WE.preventDefault event
      handleAction action
    Input input -> H.modify_ _ { input = input }
    Eval -> do
      state <- H.get
      case eval standardLibs state.input of
        Left err -> do
          H.modify_
            _
              { input = ""
              , history = state.history `snoc` { input: state.input, output: show err }
              }
        Right (Tuple value env) -> do
          H.modify_
            _
              { input = ""
              , history = state.history `snoc` { input: state.input, output: display value }
              , env = env
              }

  render state =
    HH.main_
      [ HH.h1 [] [ HH.text "REPL for the untyped lambda calculus" ]
      , HH.section_
          [ HH.h2 [] [ HH.text "Welcom to the REPL for the untyped lambda calculus." ]
          , HH.div [] $ state.history
              # map \({ input, output }) ->
                  HH.div []
                    [ HH.div []
                        [ HH.span [] [ HH.text prompt ]
                        , HH.input [ HP.disabled true, HP.value input ]
                        ]
                    , HH.div [] [ HH.text output ]
                    ]
          , HH.form [ HE.onSubmit \e -> Just $ PreventDefault e Eval ]
              [ HH.span [] [ HH.text prompt ]
              , HH.input [ HP.value state.input, HE.onValueInput $ Just <<< Input ]
              ]
          ]
      ]
    where
    prompt = "> "
