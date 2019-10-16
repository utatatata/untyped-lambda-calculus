module Editor where

import Prelude hiding (div)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HD
import UntypedLambda (Environment, eval, display, standardLibs)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event)
import Web.Event.Event as WEvent
import Web.HTML as WHtml
import Web.HTML.Window as WWin
import Web.HTML.HTMLDocument as WDoc
import Web.HTML.HTMLElement as WElem

type State
  = { input :: String
    , history :: Array { input :: String, output :: String }
    , env :: Environment
    }

data Action
  = PreventDefault Event Action
  | FocusById String
  | Input String
  | Eval

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    HD.runUI
      ( H.mkComponent
          { initialState
          , render
          , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
          }
      )
      unit
      body
  where
  initialState :: Unit -> State
  initialState _ =
    { input: ""
    , history: []
    , env: standardLibs
    }

  handleAction = case _ of
    PreventDefault event action -> do
      H.liftEffect $ WEvent.preventDefault event
      handleAction action
    FocusById id -> do
      H.liftEffect
        ( do
            win <- WHtml.window
            doc <- WWin.document win
            maybeElem <- getElementById id $ WDoc.toNonElementParentNode doc
            case maybeElem >>= WElem.fromElement of
              Nothing -> pure unit
              Just elem -> WElem.focus elem
        )
    Input input -> H.modify_ _ { input = input }
    Eval -> do
      state <- H.get
      case eval state.env state.input of
        Right (Tuple value env) -> do
          H.modify_
            _
              { input = ""
              , history = state.history `snoc` { input: state.input, output: display value }
              , env = env
              }
        Left err -> do
          H.modify_
            _
              { input = ""
              , history = state.history `snoc` { input: state.input, output: show err }
              }

  render state =
    HH.div_
      [ HH.header [ HP.classes [ ClassName "flex", ClassName "justify-between", ClassName "my-4" ] ]
          [ HH.h1 [ HP.classes [ ClassName "mx-6", ClassName "text-3xl" ] ]
              [ HH.div
                  [ HP.classes
                      [ ClassName "w-12"
                      , ClassName "h-12"
                      , ClassName "rounded-full"
                      , ClassName "bg-indigo-800"
                      , ClassName "align-middle"
                      , ClassName "text-center"
                      , ClassName "font-black"
                      ]
                  ]
                  [ HH.text "λ" ]
              ]
          , HH.nav [ HP.classes [ ClassName "mx-2" ] ]
              [ HH.ul [ HP.classes [ ClassName "flex justify-end" ] ]
                  [ HH.li_ [ HH.a [ HP.target "_blank", HP.href "https://github.com/utatatata/untyped-lambda-calculus" ] [ HH.text "GitHub" ] ]
                  ]
              ]
          ]
      , HH.main [ HP.classes [ ClassName "container", ClassName "w-auto", ClassName "mx-4" ] ]
          [ HH.section [ HP.classes [] ]
              [ HH.h2_ [ HH.text "REPL" ]
              , HH.div_ $ state.history
                  # map \({ input, output }) ->
                      HH.div_
                        [ HH.div_
                            [ HH.span_ [ HH.text prompt ]
                            , HH.input
                                [ HP.classes [ ClassName "appearance-none", ClassName "bg-gray-800", ClassName "w-11/12" ]
                                , HP.disabled true
                                , HP.value input
                                ]
                            ]
                        , HH.div_ [ HH.text output ]
                        ]
              , HH.form [ HE.onSubmit \e -> Just $ PreventDefault e Eval ]
                  [ HH.span [ HP.classes [ ClassName "mr-2" ] ] [ HH.text prompt ]
                  , HH.span [ HP.classes [ ClassName "w-11/12", ClassName "relative" ] ]
                      [ HH.input
                          [ HP.classes
                              [ ClassName "appearance-none"
                              , ClassName "bg-gray-800"
                              , ClassName "focus:outline-none"
                              , ClassName "text-gray-800"
                              ]
                          , HP.id_ "repl-input"
                          , HP.type_ InputText
                          , HP.value state.input
                          , HE.onValueInput $ Just <<< Input
                          ]
                      , HH.span
                          [ HP.classes [ ClassName "break-all", ClassName "absolute", ClassName "left-0" ]
                          , HP.spellcheck false
                          , HE.onClick $ const $ Just $ FocusById "repl-input"
                          ]
                          [ HH.text state.input ]
                      ]
                  ]
              ]
          ]
      ]
    where
    prompt = "> "
