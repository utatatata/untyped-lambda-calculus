module Editor where

import Prelude
import Data.Array (index, (:))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HD
import UntypedLambda (Environment, EvalError(..), eval, display, standardLibs)
import Web.DOM as D
import Web.DOM.Element as DE
import Web.DOM.NonElementParentNode as DN
import Web.Event.Event as WE
import Web.HTML as WH
import Web.HTML.CSSStyleDeclaration as WHS
import Web.HTML.HTMLDocument as WHD
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as WK

type State
  = { input :: String
    , inputMode :: InputMode
    , textAreaRows :: Int
    , history :: Array { input :: String, output :: String }
    , env :: Environment
    }

data InputMode
  = Inputting
  | SelectingHistory Int

data Action
  = Composition (Array Action)
  | PreventDefault WE.Event
  | Input String
  | Eval
  | KeyDown WK.KeyboardEvent
  | KeyUp WK.KeyboardEvent
  -- | ResizeTextArea
  | CalculateTextAreaRows String
  | HistoryUp
  | HistoryDown

data Id
  = TextAreaId
  | BehindTextAreaId

getString :: Id -> String
getString (TextAreaId) = "repl-input-textarea-id"

getString (BehindTextAreaId) = "behind-text-area-id"

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
    , textAreaRows: 1
    , history: []
    , env: standardLibs
    }

  handleAction = case _ of
    Composition actions ->
      for_ actions \a ->
        handleAction a
    PreventDefault e -> H.liftEffect $ WE.preventDefault e
    Input input -> do
      H.modify_ _ { input = input, inputMode = Inputting }
      handleAction $ CalculateTextAreaRows input
    Eval -> do
      state <- H.get
      case eval state.env state.input of
        Right (Tuple value env) -> do
          H.modify_
            _
              { history = { input: state.input, output: display value } : state.history
              , env = env
              }
        Left (ParseError msg) ->
          H.modify_
            _
              { history = { input: state.input, output: msg } : state.history }
      H.modify_ _ { input = "", textAreaRows = 1, inputMode = Inputting }
    KeyDown e -> case WK.key e of
      "Enter" -> handleAction $ Composition [ PreventDefault $ WK.toEvent e ]
      "ArrowUp" -> handleAction $ Composition [ PreventDefault $ WK.toEvent e, HistoryUp ]
      "ArrowDown" -> handleAction $ Composition [ PreventDefault $ WK.toEvent e, HistoryDown ]
      _ -> pure unit
    KeyUp e -> case WK.key e of
      -- onKeyDown is PC only, but onKeyUp work with both PC and smartphone (software keyboard)
      "Enter" -> handleAction $ Composition [ PreventDefault $ WK.toEvent e, Eval ]
      _ -> pure unit
    CalculateTextAreaRows input -> do
      maybeRows <-
        H.liftEffect
          $ do
              maybeElem <- byId BehindTextAreaId
              case maybeElem of
                Nothing -> pure Nothing
                Just elem -> do
                  height <- DE.scrollHeight elem
                  lineHeight <- WHS.lineHeight =<< WHS.getComputedStyle elem =<< WH.window
                  ( let
                      rows = round $ height / lineHeight

                      last = S.takeRight 1 input
                    in
                      -- consider an empty line
                      if last == "" || last == "\n" || last == "\r" then
                        pure $ Just $ rows + 1
                      else
                        pure $ Just rows
                  )
      case maybeRows of
        Nothing -> pure unit
        Just rows -> H.modify_ _ { textAreaRows = rows }
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
              -- oldest history
              Nothing -> pure unit
              Just { input, output: _ } -> do
                H.modify_ _ { input = input, inputMode = SelectingHistory n }
                handleAction $ CalculateTextAreaRows input
      )
    HistoryDown -> do
      { inputMode } <- H.get
      case inputMode of
        Inputting -> pure unit
        SelectingHistory 0 -> H.modify_ _ { input = "", textAreaRows = 1, inputMode = Inputting }
        SelectingHistory n -> do
          { history } <- H.get
          case history `index` (n - 1) of
            Nothing -> pure unit
            Just { input, output: _ } -> do
              H.modify_ _ { input = input, inputMode = SelectingHistory $ n - 1 }
              handleAction $ CalculateTextAreaRows input
    where
    byId :: Id -> Effect (Maybe D.Element)
    byId id = do
      doc <- WHW.document =<< WH.window
      DN.getElementById (getString id) $ WHD.toNonElementParentNode doc

  render state =
    HH.div_
      [ HH.header [ HP.classes [ HH.ClassName "flex", HH.ClassName "justify-between", HH.ClassName "my-4" ] ]
          [ HH.h1 [ HP.classes [ HH.ClassName "mx-6", HH.ClassName "text-3xl" ] ]
              [ HH.span
                  [ HP.classes
                      [ HH.ClassName "inline-block"
                      , HH.ClassName "w-12"
                      , HH.ClassName "h-12"
                      , HH.ClassName "rounded-full"
                      , HH.ClassName "bg-indigo-800"
                      , HH.ClassName "align-middle"
                      , HH.ClassName "text-center"
                      , HH.ClassName "font-black"
                      ]
                  ]
                  [ HH.span
                      -- pt-1 is for a design because λ appears to float a little
                      [ HP.classes [ HH.ClassName "inline-block", HH.ClassName "pt-1" ] ]
                      [ HH.text "λ" ]
                  ]
              ]
          , HH.nav [ HP.classes [ HH.ClassName "mx-2" ] ]
              [ HH.ul [ HP.classes [ HH.ClassName "flex", HH.ClassName "justify-end" ] ]
                  [ HH.li_
                      [ HH.a
                          [ HP.target "_blank", HP.href "https://github.com/utatatata/untyped-lambda-calculus" ]
                          [ HH.text "GitHub" ]
                      ]
                  ]
              ]
          ]
      , HH.main [ HP.classes [ HH.ClassName "mx-4", HH.ClassName "w-auto" ] ]
          [ HH.section_
              [ HH.h2_ [ HH.text "REPL" ]
              , HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "flex-col-reverse" ] ] $ state.history
                  # map \({ input, output }) ->
                      HH.div_
                        [ HH.div [ HP.classes [ HH.ClassName "flex" ] ]
                            [ HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "justify-env" ] ]
                                [ HH.span [ HP.classes [ HH.ClassName "mr-1" ] ]
                                    [ HH.text prompt ]
                                ]
                            , HH.div [ HP.classes [ HH.ClassName "w-full", HH.ClassName "bg-gray-800" ] ]
                                [ HH.span [ HP.classes [ HH.ClassName "break-all", HH.ClassName "whitespace-pre-wrap" ] ]
                                    [ HH.text input ]
                                ]
                            ]
                        , HH.div [ HP.classes [ HH.ClassName "break-all" ] ] [ HH.text output ]
                        ]
              , HH.div [ HP.classes [ HH.ClassName "flex" ] ]
                  [ HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "justify-end" ] ]
                      [ HH.span [ HP.classes [ HH.ClassName "mr-1" ] ] [ HH.text prompt ] ]
                  , HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "relative", HH.ClassName "w-full" ] ]
                      [ HH.textarea
                          [ HP.id_ $ getString TextAreaId
                          , HP.classes
                              [ HH.ClassName "w-full"
                              , HH.ClassName "apperance-none"
                              , HH.ClassName "focus:outline-none"
                              , HH.ClassName "bg-gray-800"
                              , HH.ClassName "resize-none"
                              -- prevent the appearance of scroll bar
                              , HH.ClassName "overflow-y-hidden"
                              -- to hide a absolute div below
                              , HH.ClassName "z-10"
                              ]
                          , HP.rows state.textAreaRows
                          , HP.autofocus true
                          , HP.value state.input
                          , HE.onKeyDown $ Just <<< KeyDown
                          , HE.onKeyUp $ Just <<< KeyUp
                          , HE.onValueInput $ Just <<< Input
                          ]
                      , HH.div
                          [ HP.id_ $ getString BehindTextAreaId
                          , HP.classes
                              [ HH.ClassName "absolute"
                              , HH.ClassName "w-full"
                              , HH.ClassName "break-all"
                              , HH.ClassName "bg-gray-900"
                              , HH.ClassName "text-gray-900"
                              , HH.ClassName "whitespace-pre-wrap"
                              ]
                          ]
                          [ HH.text state.input ]
                      ]
                  ]
              ]
          ]
      ]
    where
    prompt = ">"
