module Editor where

import Prelude hiding (div)
import CSS (left) as CSS
import CSS.Size (rem) as CSS
import Data.Array ((:), index, mapWithIndex, uncons)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (length, take, drop)
import Data.String.CodeUnits (singleton, toCharArray, splitAt)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Core as HHCore
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
import Web.UIEvent.MouseEvent as WEventMouse

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
  = Composition Action Action
  | PreventDefault Event Action
  | StopPropagation Event Action
  | FocusById String
  | HistoryUp
  | HistoryDown
  | MoveCursor Int
  | MoveCursorLeft
  | MoveCursorRight
  | KeyDown KeyboardEvent
  | Insert Char
  | Backspace
  | Delete
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
    Composition x y -> do
      handleAction x
      handleAction y
    PreventDefault event action -> do
      H.liftEffect $ WEvent.preventDefault event
      handleAction action
    StopPropagation event action -> do
      H.liftEffect $ WEvent.stopPropagation event
      handleAction action
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
              Just { input, output: _ } -> H.modify_ _ { inputMode = SelectingHistory n, input = input, cursorPos = length input }
      )
    HistoryDown -> do
      { inputMode } <- H.get
      case inputMode of
        Inputting -> pure unit
        SelectingHistory 0 -> H.modify_ _ { inputMode = Inputting, input = "", cursorPos = 0 }
        SelectingHistory n -> do
          { history } <- H.get
          case history `index` (n - 1) of
            Nothing -> pure unit
            Just { input, output: _ } -> H.modify_ _ { inputMode = SelectingHistory $ n - 1, input = input, cursorPos = length input }
    MoveCursor n -> do
      len <- length <<< (_.input) <$> H.get
      H.modify_ _ { cursorPos = max 0 $ min len n }
    MoveCursorLeft -> do
      { cursorPos } <- H.get
      handleAction $ MoveCursor $ cursorPos - 1
    MoveCursorRight -> do
      { cursorPos } <- H.get
      handleAction $ MoveCursor $ cursorPos + 1
    KeyDown e -> do
      case WEventKey.key e of
        "ArrowUp" -> handleAction HistoryUp
        "ArrowDown" -> handleAction HistoryDown
        "ArrowLeft" -> handleAction MoveCursorLeft
        "ArrowRight" -> handleAction MoveCursorRight
        "Backspace" -> handleAction Backspace
        "Delete" -> handleAction Delete
        "Enter" -> handleAction Eval
        key -> case uncons $ toCharArray key of
          Just { head: c, tail: [] } -> handleAction $ Insert c
          _ -> pure unit
    Insert c -> do
      { cursorPos, input } <- H.get
      ( let
          { before, after } = splitAt cursorPos input
        in
          H.modify_ _ { input = before <> (singleton c) <> after, cursorPos = cursorPos + 1 }
      )
    Backspace -> do
      { cursorPos, input } <- H.get
      H.modify_ _ { input = take (cursorPos - 1) input <> drop cursorPos input, cursorPos = max 0 (cursorPos - 1) }
    Delete -> do
      { cursorPos, input } <- H.get
      H.modify_ _ { input = take cursorPos input <> drop (cursorPos + 1) input }
    Eval -> do
      state <- H.modify _ { inputMode = Inputting }
      case eval state.env state.input of
        Right (Tuple value env) -> do
          H.modify_
            _
              { input = ""
              , inputMode = Inputting
              , cursorPos = 0
              , history = { input: state.input, output: display value } : state.history
              , env = env
              }
        Left err -> do
          H.modify_
            _
              { input = ""
              , inputMode = Inputting
              , cursorPos = 0
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
              , HH.div [ HP.class_ $ ClassName "flex" ]
                  [ HH.div [ HP.classes [ ClassName "flex", ClassName "justify-end" ] ]
                      [ HH.span [ HP.class_ $ ClassName "mr-1" ] [ HH.text prompt ] ]
                  , HH.div
                      [ HP.classes [ ClassName "select-none", ClassName "relative", ClassName "w-full", ClassName "bg-gray-800" ]
                      , HP.prop (HHCore.PropName "setStyle") "left: -0.25rem;"
                      , HE.onKeyDown $ Just <<< KeyDown
                      , HE.onClick $ const $ Just $ Composition (FocusById hiddenReplInputId) (MoveCursor $ length state.input)
                      ]
                      [ HH.div
                          [ HP.classes [ ClassName "pointer-events-none", ClassName "absolute" ]
                          , HCSS.style do CSS.left $ CSS.rem $ -0.25 + 0.5 * (toNumber state.cursorPos)
                          ]
                          [ HH.span [] [ HH.text "|" ] ]
                      , HH.div
                          []
                          [ HH.input
                              [ HP.id_ hiddenReplInputId
                              , HP.autofocus true
                              , HP.classes
                                  [ ClassName "absolute"
                                  , ClassName "w-0"
                                  , ClassName "h-0"
                                  , ClassName "appearance-none"
                                  , ClassName "bg-gray-900"
                                  , ClassName "focus:outline-none"
                                  , ClassName "text-gray-900"
                                  ]
                              ]
                          ]
                      , HH.div [ HP.classes [ ClassName "flex" ] ] $ toCharArray state.input
                          # mapWithIndex \i c ->
                              HH.span
                                [ HP.class_ $ ClassName "w-2"
                                , HE.onClick \e -> Just $ Composition (FocusById hiddenReplInputId) (StopPropagation (WEventMouse.toEvent e) $ MoveCursor $ i + 1)
                                ]
                                [ HH.text $ singleton c ]
                      ]
                  ]
              ]
          ]
      ]
    where
    prompt = "> "

    hiddenReplInputId = "hidden-repl-input"
