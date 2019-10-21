module Editor where

import Prelude
import Data.Array (index, length, (:))
import Data.Display (display)
import Data.Foldable (for_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits as S
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core as HHC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HD
import UntypedLambda.REPL as R
import Web.DOM as D
import Web.DOM.Element as DE
import Web.DOM.NonElementParentNode as DN
import Web.Event.Event as WE
import Web.HTML as WH
import Web.HTML.CSSStyleDeclaration as WHS
import Web.HTML.HTMLDocument as WHD
import Web.HTML.HTMLElement as WHE
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as WK
import Web.UIEvent.MouseEvent as WM

type State
  = { input :: String
    , inputMode :: InputMode
    , textAreaRows :: Int
    , repl :: R.REPL
    , tab :: Tab
    }

data InputMode
  = Inputting
  | SelectingHistory Int

data Tab
  = REPL
  | Environment
  | Help

data Action
  = Composition (Array Action)
  | PreventDefault WE.Event
  | Input String
  | Eval
  | KeyDown WK.KeyboardEvent
  | KeyUp WK.KeyboardEvent
  | CalculateTextAreaRows String
  | HistoryUp
  | HistoryDown
  | SwitchTab Tab
  | FocusById Id

data Id
  = TextAreaId
  | BehindTextAreaId

getString :: Id -> String
getString (TextAreaId) = "repl-input-textarea-id"

getString (BehindTextAreaId) = "behind-text-area-id"

data InputLineMode w i
  = Dummy (HHC.HTML w i)
  | TextArea PromptMode

data PromptMode
  = WithPrompt
  | WithoutPrompt

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
    , repl: R.init R.standardLibs
    , tab: REPL
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
      { input, repl } <- H.get
      H.modify_
        _
          { input = ""
          , textAreaRows = 1
          , inputMode = Inputting
          , repl = R.eval input repl
          }
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
            { repl: R.REPL { history } } <- H.get
            case history `index` ((length history) - n - 1) of
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
          { repl: R.REPL { history } } <- H.get
          case history `index` ((length history) - n) of
            Nothing -> pure unit
            Just { input, output: _ } -> do
              H.modify_ _ { input = input, inputMode = SelectingHistory $ n - 1 }
              handleAction $ CalculateTextAreaRows input
    SwitchTab tab -> H.modify_ _ { tab = tab }
    FocusById id ->
      H.liftEffect do
        maybeElem <- byId id
        case WHE.fromElement =<< maybeElem of
          Just elem -> WHE.focus elem
          Nothing -> pure unit
    where
    byId :: Id -> Effect (Maybe D.Element)
    byId id = do
      doc <- WHW.document =<< WH.window
      DN.getElementById (getString id) $ WHD.toNonElementParentNode doc

  render state =
    let
      repl = unwrap state.repl
    in
      HH.div_
        [ HH.header [ HP.classes [ HH.ClassName "flex", HH.ClassName "justify-between", HH.ClassName "my-4" ] ]
            [ HH.h1 [ HP.classes [ HH.ClassName "mx-6", HH.ClassName "text-3xl" ] ]
                [ HH.span
                    [ HP.classes
                        -- logo
                        [ HH.ClassName "inline-block"
                        , HH.ClassName "w-12"
                        , HH.ClassName "h-12"
                        , HH.ClassName "rounded-full"
                        , HH.ClassName "bg-indigo-800"
                        , HH.ClassName "align-middle"
                        , HH.ClassName "text-center"
                        , HH.ClassName "font-black"
                        , HH.ClassName "font-mono"
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
        , HH.main
            [ HP.classes
                [ HH.ClassName "mx-4"
                , HH.ClassName "w-auto"
                , HH.ClassName "border-2"
                , HH.ClassName "border-gray-800"
                , HH.ClassName "rounded"
                , HH.ClassName "p-2"
                ]
            ]
            [ HH.ul
                [ HP.classes
                    [ HH.ClassName "flex"
                    , HH.ClassName "mb-5"
                    , HH.ClassName "text-gray-400"
                    ]
                ]
                let
                  active = [ HH.ClassName "text-gray-700" ]
                in
                  [ HH.li
                      [ HP.classes $ [ HH.ClassName "mr-5" ]
                          <> case state.tab of
                              REPL -> active
                              _ -> []
                      ]
                      [ HH.a
                          [ HP.classes [ HH.ClassName "cursor-default" ]
                          , HE.onClick \e -> Just $ Composition [ PreventDefault $ WM.toEvent e, SwitchTab REPL, FocusById TextAreaId ]
                          ]
                          [ HH.text "REPL" ]
                      ]
                  , HH.li
                      [ HP.classes $ [ HH.ClassName "mr-5" ]
                          <> case state.tab of
                              Environment -> active
                              _ -> []
                      ]
                      [ HH.a
                          [ HP.classes [ HH.ClassName "cursor-default" ]
                          , HE.onClick \e -> Just $ Composition [ PreventDefault $ WM.toEvent e, SwitchTab Environment ]
                          ]
                          [ HH.text "Environment" ]
                      ]
                  , HH.li
                      [ HP.classes
                          $ case state.tab of
                              Help -> active
                              _ -> []
                      ]
                      [ HH.a
                          [ HP.classes [ HH.ClassName "cursor-default" ]
                          , HE.onClick \e -> Just $ Composition [ PreventDefault $ WM.toEvent e, SwitchTab Help ]
                          ]
                          [ HH.text "Help" ]
                      ]
                  ]
            , HH.section
                [ HP.classes
                    $ case state.tab of
                        REPL -> []
                        _ -> [ HH.ClassName "hidden" ]
                ]
                [ HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "flex-col" ] ] $ repl.history
                    # map \({ input, output }) ->
                        HH.div_
                          $ [ inputLine [] $ Dummy
                                $ HH.span
                                    [ HP.classes [ HH.ClassName "break-all", HH.ClassName "whitespace-pre-wrap" ] ]
                                    [ HH.text input ]
                            ]
                          <> case output of
                              Just str ->
                                [ HH.div
                                    [ HP.classes [ HH.ClassName "mb-4", HH.ClassName "break-all", HH.ClassName "whitespace-pre-wrap", HH.ClassName "font-mono" ]
                                    ]
                                    [ HH.text str ]
                                ]
                              Nothing -> []
                , inputLine
                    ( case repl.inputMode of
                        R.Singleline -> [ HH.ClassName "hidden" ]
                        R.Multiline ->
                          if repl.inputPool == [] then
                            [ HH.ClassName "hidden" ]
                          else
                            []
                    )
                    $ Dummy
                    $ HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "flex-col" ] ]
                    $ repl.inputPool
                    # map \input ->
                        HH.span [ HP.classes [ HH.ClassName "break-all", HH.ClassName "whitespace-pre-wrap" ] ]
                          [ HH.text input ]
                , inputLine [] $ TextArea
                    $ case repl.inputMode of
                        R.Singleline -> WithPrompt
                        R.Multiline ->
                          if repl.inputPool == [] then
                            WithPrompt
                          else
                            WithoutPrompt
                ]
            , HH.section
                [ HP.classes
                    $ case state.tab of
                        Environment -> []
                        _ -> [ HH.ClassName "hidden" ]
                ]
                [ HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "flex-col" ] ] $ repl.env
                    # map \(Tuple name value) ->
                        HH.div [ HP.classes [ HH.ClassName "break-all" ] ] [ HH.text $ name <> " = " <> display value ]
                ]
            , HH.section
                [ HP.classes
                    $ case state.tab of
                        Help -> []
                        _ -> [ HH.ClassName "hidden" ]
                ]
                [ HH.h2 [ HP.classes [ HH.ClassName "text-xl", HH.ClassName "mb-4" ] ] [ HH.text "REPL for The Untyped λ Calculus" ]
                , HH.section [ HP.classes [ HH.ClassName "mb-8" ] ]
                    [ HH.h3 [ HP.classes [ HH.ClassName "text-lg", HH.ClassName "mb-2" ] ] [ HH.text "Interpreter Options" ]
                    , HH.div [ HP.classes [ HH.ClassName "ml-2", HH.ClassName "break-words" ] ] [ HH.text "Type `.help` in REPL for help." ]
                    ]
                , HH.section [ HP.classes [ HH.ClassName "mb-8" ] ]
                    [ HH.h3 [ HP.classes [ HH.ClassName "text-lg", HH.ClassName "mb-2" ] ] [ HH.text "Syntax of λ expression" ]
                    , HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "ml-4", HH.ClassName "mt-2", HH.ClassName "mb-4" ] ]
                        [ HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "flex-col", HH.ClassName "mr-2" ] ]
                            [ HH.div [] [ HH.text "<expression> ::=" ]
                            , HH.div [] [ HH.text "<expression> ::=" ]
                            , HH.div [] [ HH.text "<expression> ::=" ]
                            ]
                        , HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "flex-col" ] ]
                            [ HH.div [] [ HH.text "<variable>" ]
                            , HH.div [] [ HH.text "<lambda abstraction>" ]
                            , HH.div [] [ HH.text "<application>" ]
                            ]
                        ]
                    , HH.section [ HP.classes [ HH.ClassName "mb-4" ] ]
                        [ HH.h4 [ HP.classes [ HH.ClassName "mb-2" ] ] [ HH.text "1. Variables" ]
                        , HH.div [ HP.classes [ HH.ClassName "ml-4", HH.ClassName "mt-2", HH.ClassName "mb-4" ] ]
                            [ HH.text "<variable> ::= <identifier>" ]
                        , HH.div [ HP.classes [ HH.ClassName "ml-2", HH.ClassName "break-words" ] ]
                            [ HH.p [] [ HH.text "<identifier> consists of alphabets, digits, ASCII characters except 'λ', and symbols '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '<', '>', '/', '?'." ]
                            ]
                        ]
                    , HH.section [ HP.classes [ HH.ClassName "mb-4" ] ]
                        [ HH.h4 [ HP.classes [ HH.ClassName "mb-2" ] ] [ HH.text "2. Lambda Abstractions" ]
                        , HH.div [ HP.classes [ HH.ClassName "ml-4", HH.ClassName "mt-2", HH.ClassName "mb-4" ] ]
                            [ HH.div [] [ HH.text "<lambda abstraction> :: = (λ<identifier>.<expression>)" ]
                            ]
                        , HH.div [ HP.classes [ HH.ClassName "ml-2", HH.ClassName "break-words" ] ]
                            [ HH.p [] [ HH.text "Lambda abstractions are right-associative." ]
                            , HH.p [] [ HH.text "`(λx.λy.M)` is equivalent to `(λx.(λy.M))`." ]
                            , HH.p [] [ HH.text "`(λx y.M)` is abbreviation of `(λx.(λy.M))`." ]
                            ]
                        ]
                    , HH.section [ HP.classes [ HH.ClassName "mb-4" ] ]
                        [ HH.h4 [ HP.classes [ HH.ClassName "mb-2" ] ] [ HH.text "3. Applications" ]
                        , HH.div [ HP.classes [ HH.ClassName "ml-4", HH.ClassName "mt-2", HH.ClassName "mb-4" ] ]
                            [ HH.div [] [ HH.text "<application> ::= (<expression> <expression>)" ]
                            ]
                        , HH.div [ HP.classes [ HH.ClassName "ml-2", HH.ClassName "break-words" ] ]
                            [ HH.p [] [ HH.text "Applications are left-associative." ]
                            , HH.p [] [ HH.text "`(M N L)` is equivalent to `((M N) L)`." ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    where
    inputLine classes mode = case mode of
      Dummy input ->
        HH.div [ HP.classes $ [ HH.ClassName "flex", HH.ClassName "font-mono" ] <> classes ]
          $ prompt WithPrompt
          : [ HH.div [ HP.classes [ HH.ClassName "w-full", HH.ClassName "bg-gray-800" ] ]
                [ input ]
            ]
      TextArea withPrompt ->
        HH.div [ HP.classes $ [ HH.ClassName "flex", HH.ClassName "font-mono" ] <> classes ]
          $ prompt withPrompt
          : [ HH.div
                [ HP.classes
                    $ [ HH.ClassName "flex", HH.ClassName "relative", HH.ClassName "w-full" ]
                ]
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
                        , HH.ClassName "break-all"
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
      where
      prompt withPrompt =
        HH.div
          [ HP.classes
              $ [ HH.ClassName "flex", HH.ClassName "justify-end" ]
              <> case withPrompt of
                  WithPrompt -> []
                  WithoutPrompt -> [ HH.ClassName "invisible" ]
          ]
          [ HH.span [ HP.classes [ HH.ClassName "mr-1" ] ]
              [ HH.text ">" ]
          ]
