module Editor where

import Prelude hiding (div)
import CSS as C
import CSS.Size as CSize
import Data.Array (index, mapWithIndex, snoc, uncons, (:))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodeUnits (singleton, toCharArray, splitAt)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Core as HHCore
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HD
import UntypedLambda (Environment, eval, display, standardLibs)
import Web.DOM (Element)
import Web.DOM.Element as WDElem
import Web.DOM.Node as WDNode
import Web.DOM.NodeList as WDNodeList
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event)
import Web.Event.Event as WEvent
import Web.HTML as WH
import Web.HTML.HTMLDocument as WDoc
import Web.HTML.HTMLElement as WHElem
import Web.HTML.Window as WHWin
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as WUIEKey
import Web.UIEvent.MouseEvent as WUIEMouse

type State
  = { input :: String
    , inputMode :: InputMode
    , cursorPos :: Maybe CursorPos
    , history :: Array { input :: String, output :: String }
    , env :: Environment
    }

data InputMode
  = Inputting
  | SelectingHistory Int

type CursorPos
  = { col :: Int
    , left :: Number
    , top :: Number
    }

data Action
  = Composition (Array Action)
  | PreventDefault Event
  | StopPropagation Event
  | FocusById Id
  | HistoryUp
  | HistoryDown
  | HideCursor
  | MoveCursor Int
  | MoveCursorLeft
  | MoveCursorRight
  | KeyDown KeyboardEvent
  | Insert Char
  | Backspace
  | Delete
  | Eval

data Id
  = HiddenReplInput
  | InputLine

toString :: Id -> String
toString HiddenReplInput = "hidden-repl-input"

toString InputLine = "input-line"

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
    , cursorPos: Just { col: 0, left: 0.0, top: 0.0 }
    , history: []
    , env: standardLibs
    }

  handleAction = case _ of
    Composition actions -> do
      for_ actions \a ->
        handleAction a
    PreventDefault event -> do
      H.liftEffect $ WEvent.preventDefault event
    StopPropagation event -> do
      H.liftEffect $ WEvent.stopPropagation event
    FocusById id -> do
      H.liftEffect
        ( do
            maybeElem <- byId id
            case maybeElem >>= WHElem.fromElement of
              Nothing -> do
                pure unit
              Just html -> do
                WHElem.focus html
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
              Just { input, output: _ } -> do
                H.modify_
                  _
                    { inputMode = SelectingHistory n
                    , input = input
                    }
                handleAction $ MoveCursor $ S.length input
      )
    HistoryDown -> do
      { inputMode } <- H.get
      case inputMode of
        Inputting -> pure unit
        SelectingHistory 0 -> do
          H.modify_
            _
              { inputMode = Inputting
              , input = ""
              }
          handleAction $ MoveCursor 0
        SelectingHistory n -> do
          { history } <- H.get
          case history `index` (n - 1) of
            Nothing -> pure unit
            Just { input, output: _ } -> do
              H.modify_
                _
                  { inputMode = SelectingHistory $ n - 1
                  , input = input
                  }
              handleAction $ MoveCursor $ S.length input
    HideCursor -> H.modify_ _ { cursorPos = Nothing }
    MoveCursor n -> do
      { input } <- H.get
      ( let
          col = max 0 $ min (S.length input) n
        in
          do
            maybeOffset <-
              H.liftEffect
                ( do
                    maybeElem <- byId InputLine
                    case maybeElem of
                      Nothing -> pure Nothing
                      Just lineInput -> do
                        children <- WDElem.toNode lineInput # WDNode.childNodes >>= WDNodeList.toArray
                        case (children `index` col >>= WDElem.fromNode >>= WHElem.fromElement) of
                          Nothing -> pure Nothing
                          Just cursorChar -> Just <$> ({ left: _, top: _ } <$> WHElem.offsetLeft cursorChar <*> WHElem.offsetTop cursorChar)
                )
            case maybeOffset of
              Nothing -> do
                pure unit
              Just { left, top } -> H.modify_ _ { cursorPos = Just { col: col, left: left, top: top } }
      )
    MoveCursorLeft -> do
      { cursorPos } <- H.get
      case cursorPos of
        Nothing -> pure unit
        Just { col, left: _, top: _ } -> handleAction $ MoveCursor $ col - 1
    MoveCursorRight -> do
      { cursorPos } <- H.get
      case cursorPos of
        Nothing -> pure unit
        Just { col, left: _, top: _ } -> handleAction $ MoveCursor $ col + 1
    KeyDown e -> do
      case WUIEKey.key e of
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
      case cursorPos of
        Nothing -> pure unit
        Just { col, left: _, top: _ } ->
          ( let
              { before, after } = splitAt col input
            in
              do
                H.modify_ _ { input = before <> (singleton c) <> after, inputMode = Inputting }
                handleAction $ MoveCursor $ col + 1
          )
    Backspace -> do
      { cursorPos, input } <- H.get
      case cursorPos of
        Nothing -> pure unit
        Just { col, left: _, top: _ } -> do
          H.modify_ _ { input = S.take (col - 1) input <> S.drop col input, inputMode = Inputting }
          handleAction $ MoveCursor $ col - 1
    Delete -> do
      { cursorPos, input } <- H.get
      case cursorPos of
        Nothing -> pure unit
        Just { col, left: _, top: _ } -> H.modify_ _ { input = S.take col input <> S.drop (col + 1) input, inputMode = Inputting }
    Eval -> do
      state <- H.get
      case eval state.env state.input of
        Right (Tuple value env) -> do
          H.modify_
            _
              { history = { input: state.input, output: display value } : state.history
              , env = env
              }
        Left err -> do
          H.modify_
            _
              { history = { input: state.input, output: show err } : state.history }
      H.modify_ _ { input = "", inputMode = Inputting }
      handleAction $ MoveCursor 0
    where
    byId :: Id -> Effect (Maybe Element)
    byId id = do
      doc <- WHWin.document =<< WH.window
      getElementById (toString id) $ WDoc.toNonElementParentNode doc

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
                            , HH.div [ HP.classes [ ClassName "w-full", ClassName "bg-gray-800" ] ]
                                [ HH.span
                                    [ HP.classes [ ClassName "break-all" ]
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
                      , HE.onClick $ const $ Just $ Composition [ FocusById HiddenReplInput, MoveCursor $ S.length state.input ]
                      ]
                      [ HH.div
                          ( case state.cursorPos of
                              Nothing -> [ HP.classes [ ClassName "pointer-events-none", ClassName "absolute", ClassName "invisible" ] ]
                              Just { col: _, left, top } ->
                                [ HP.classes [ ClassName "pointer-events-none", ClassName "absolute" ]
                                , HC.style do
                                    C.left $ CSize.px left
                                    C.top $ CSize.px top
                                    -- offset
                                    C.marginLeft $ CSize.rem (-0.2)
                                ]
                          )
                          [ HH.span [] [ HH.text "|" ] ]
                      , HH.div
                          []
                          [ HH.input
                              [ HP.id_ $ toString HiddenReplInput
                              , HP.type_ InputText
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
                              , HE.onBlur $ const $ Just $ HideCursor
                              ]
                          ]
                      , HH.div [ HP.id_ $ toString InputLine, HP.classes [ ClassName "flex", ClassName "flex-wrap" ] ]
                          $ ( toCharArray state.input
                                # mapWithIndex \i c ->
                                    HH.span
                                      [ HP.class_ $ ClassName "w-2"
                                      , HE.onClick \e -> Just $ Composition [ FocusById HiddenReplInput, StopPropagation $ WUIEMouse.toEvent e, MoveCursor $ i + 1 ]
                                      ]
                                      [ HH.text $ singleton c ]
                            )
                              `snoc`
                                HH.span [ HP.classes [ ClassName "w-0" ] ] []
                      ]
                  ]
              ]
          ]
      ]
    where
    prompt = "> "
