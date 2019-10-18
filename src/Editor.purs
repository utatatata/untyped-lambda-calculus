module Editor where

import Prelude
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (round)
import Data.Maybe (Maybe(..))
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
import Web.HTML as WH
import Web.HTML.CSSStyleDeclaration as WHS
import Web.HTML.HTMLDocument as WHD
import Web.HTML.HTMLTextAreaElement as WHTA
import Web.HTML.Window as WHW
import Web.UIEvent.KeyboardEvent as WK

type State
  = { input :: String
    , inputMode :: InputMode
    , textareaRows :: Int
    , history :: Array { input :: String, output :: String }
    , env :: Environment
    }

data InputMode
  = Inputting
  | SelectingHistory Int

data Action
  = Composition (Array Action)
  | Input String
  | Eval
  | KeyDown WK.KeyboardEvent
  | ResizeTextArea

data Id
  = TextareaId

getString :: Id -> String
getString (TextareaId) = "repl-input-textarea-id"

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
    , textareaRows: 1
    , history: []
    , env: standardLibs
    }

  handleAction = case _ of
    Composition actions ->
      for_ actions \a ->
        handleAction a
    Input input -> H.modify_ _ { input = input }
    Eval -> do
      state <- H.get
      case eval state.env state.input of
        Right (Tuple value env) ->
          H.modify_
            _
              { history = { input: state.input, output: display value } : state.history
              , env = env
              }
        Left (ParseError msg) ->
          H.modify_
            _
              { history = { input: state.input, output: msg } : state.history }
      H.modify_ _ { input = "", inputMode = Inputting }
    KeyDown e -> case WK.key e of
      "Enter" ->
        if WK.ctrlKey e || WK.shiftKey e || WK.altKey e || WK.metaKey e then
          handleAction Eval
        else
          pure unit
      _ -> pure unit
    ResizeTextArea -> do
      maybeRows <-
        H.liftEffect
          $ do
              maybeElem <- byId TextareaId
              case maybeElem of
                Nothing -> pure Nothing
                Just elem -> do
                  -- reset rows: force render?
                  case WHTA.fromElement elem of
                    Nothing -> pure unit
                    Just e -> WHTA.setRows 1 e
                  height <- DE.scrollHeight elem
                  lineHeight <- WHS.lineHeight =<< WHS.getComputedStyle elem =<< WH.window
                  pure $ Just $ round $ height / lineHeight
      case maybeRows of
        Nothing -> pure unit
        Just rows -> do
          H.modify_ _ { textareaRows = rows }
    where
    byId :: Id -> Effect (Maybe D.Element)
    byId id = do
      doc <- WHW.document =<< WH.window
      DN.getElementById (getString id) $ WHD.toNonElementParentNode doc

  render state =
    HH.div_
      [ HH.header [ HP.classes [ HH.ClassName "flex", HH.ClassName "justify-between", HH.ClassName "my-4" ] ]
          [ HH.h1 [ HP.classes [ HH.ClassName "mx-6", HH.ClassName "text-3xl" ] ]
              [ HH.div
                  [ HP.classes
                      [ HH.ClassName "w-12"
                      , HH.ClassName "h-12"
                      , HH.ClassName "rounded-full"
                      , HH.ClassName "bg-indigo-800"
                      , HH.ClassName "align-middle"
                      , HH.ClassName "text-center"
                      , HH.ClassName "font-black"
                      ]
                  ]
                  [ HH.text "λ" ]
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
                                [ HH.span [ HP.classes [ HH.ClassName "break-all" ] ]
                                    [ HH.text input ]
                                ]
                            ]
                        , HH.div [ HP.classes [ HH.ClassName "break-all" ] ] [ HH.text output ]
                        ]
              , HH.div [ HP.classes [ HH.ClassName "flex" ] ]
                  [ HH.div [ HP.classes [ HH.ClassName "flex", HH.ClassName "justify-end" ] ]
                      [ HH.span [ HP.classes [ HH.ClassName "mr-1" ] ] [ HH.text prompt ] ]
                  , HH.textarea
                      [ HP.id_ $ getString TextareaId
                      , HP.classes
                          [ HH.ClassName "w-full"
                          , HH.ClassName "apperance-none"
                          , HH.ClassName "focus:outline-none"
                          , HH.ClassName "bg-gray-800"
                          , HH.ClassName "resize-none"
                          ]
                      , HP.rows state.textareaRows
                      , HP.autofocus true
                      , HP.value state.input
                      , HE.onKeyDown $ Just <<< KeyDown
                      , HE.onValueInput \str -> Just $ Composition [ Input str, ResizeTextArea ]
                      ]
                  ]
              ]
          ]
      ]
    where
    prompt = ">"
