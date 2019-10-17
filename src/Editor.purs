module Editor where

import Prelude hiding (div)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array ((:), index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
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
import Web.HTML.HTMLDocument as WDoc
import Web.HTML.HTMLElement as WElem
import Web.HTML.Window as WWin
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as WEventKey

type State
  = { input :: String
    , inputMode :: InputMode
    , cursorPos :: Int
    , history :: Array { input :: String, output :: String }
    , env :: Environment
    }

data InputMode
  = Inputting
  | SelectingHistory Int

data Action
  = PreventDefault Event Action
  | StopPropagation Event Action
  | FocusById String
  | HistoryUp
  | HistoryDown
  | MoveCursor Int
  | KeyDown KeyboardEvent
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
    , inputMode: Inputting
    , cursorPos: 0
    , history: []
    , env: standardLibs
    }

  handleAction = case _ of
    PreventDefault event action -> do
      H.liftEffect $ WEvent.preventDefault event
      handleAction action
    StopPropagation event action -> do
      H.liftEffect $ WEvent.stopPropagation event
      handleAction action
    -- REMOVE
    FocusById id ->
      H.liftEffect
        ( do
            win <- WHtml.window
            doc <- WWin.document win
            maybeElem <- getElementById id $ WDoc.toNonElementParentNode doc
            case maybeElem >>= WElem.fromElement of
              Nothing -> pure unit
              Just elem -> WElem.focus elem
        )
    HistoryUp -> do
      { inputMode } <- H.get
      ( let
          n = case inputMode of
            Inputting -> 0
            SelectingHistory current -> current + 1
        in
          do
            { history } <- H.get
            case history `index` n of
              Nothing -> pure unit
              Just { input, output: _ } -> H.modify_ _ { inputMode = SelectingHistory n, input = input }
      )
    HistoryDown -> do
      { inputMode } <- H.get
      case inputMode of
        Inputting -> pure unit
        SelectingHistory 0 -> H.modify_ _ { inputMode = Inputting, input = "" }
        SelectingHistory n -> do
          { history } <- H.get
          case history `index` (n - 1) of
            Nothing -> pure unit
            Just { input, output: _ } -> H.modify_ _ { inputMode = SelectingHistory $ n - 1, input = input }
    MoveCursor n -> do
      len <- length <<< (_.input) <$> H.get
      H.modify_ _ { cursorPos = min len n }
    KeyDown e -> do
      H.liftEffect $ log $ WEventKey.key e <> " " <> WEventKey.code e
      case WEventKey.key e of
        "ArrowUp" -> handleAction HistoryUp
        "ArrowDown" -> handleAction HistoryDown
        _ -> pure unit
    Input input -> H.modify_ _ { inputMode = Inputting, input = input }
    Eval -> do
      state <- H.modify _ { inputMode = Inputting }
      case eval state.env state.input of
        Right (Tuple value env) -> do
          H.modify_
            _
              { input = ""
              , history = { input: state.input, output: display value } : state.history
              , env = env
              }
        Left err -> do
          H.modify_
            _
              { input = ""
              , history = { input: state.input, output: show err } : state.history
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
      , HH.main [ HP.classes [ ClassName "mx-4", ClassName "w-auto" ] ]
          [ HH.section_
              [ HH.h2_ [ HH.text "REPL" ]
              , HH.div [ HP.classes [ ClassName "flex", ClassName "flex-col-reverse" ] ] $ state.history
                  # map \({ input, output }) ->
                      HH.div_
                        [ HH.div [ HP.class_ $ ClassName "flex" ]
                            [ HH.div [ HP.classes [ ClassName "flex", ClassName "justify-end" ] ]
                                [ HH.span [ HP.class_ $ ClassName "mr-1" ] [ HH.text prompt ] ]
                            , HH.div [ HP.class_ $ ClassName "w-full" ]
                                [ HH.span
                                    [ HP.classes [ ClassName "break-all", ClassName "bg-gray-800", ClassName "w-full" ]
                                    ]
                                    [ HH.text input ]
                                ]
                            ]
                        , HH.div [ HP.class_ $ ClassName "break-all" ] [ HH.text output ]
                        ]
              , HH.form [ HP.class_ $ ClassName "flex", HE.onSubmit \e -> Just $ PreventDefault e Eval ]
                  [ HH.div [ HP.classes [ ClassName "flex", ClassName "justify-end" ] ]
                      [ HH.span [ HP.class_ $ ClassName "mr-1" ] [ HH.text prompt ] ]
                  , HH.div [ HP.class_ $ ClassName "w-full" ]
                      [ HH.span [ HP.classes [ ClassName "relative" ] ]
                          [ HH.input
                              [ HP.classes
                                  [ ClassName "w-full"
                                  , ClassName "appearance-none"
                                  , ClassName "bg-gray-800"
                                  , ClassName "focus:outline-none"
                                  , ClassName "text-gray-800"
                                  ]
                              , HP.id_ "repl-input"
                              , HP.type_ InputText
                              , HP.autocomplete false
                              , HP.value state.input
                              , HE.onValueInput $ Just <<< Input
                              , HE.onKeyDown $ Just <<< KeyDown
                              ]
                          , HH.span
                              [ HP.classes [ ClassName "break-all", ClassName "absolute", ClassName "left-0", ClassName "bg-gray-800" ]
                              , HP.spellcheck false
                              , HE.onClick $ const $ Just $ FocusById "repl-input"
                              ]
                              [ HH.text state.input ]
                          ]
                      ]
                  ]
              ]
          ]
      ]
    where
    prompt = "> "
