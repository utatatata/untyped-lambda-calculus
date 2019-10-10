module Editor where

import Prelude hiding (div)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen (mkComponent, mkEval, defaultEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (ClassName(..), div, h1, h2, h3, text, section_, main_, ul, li_)
import Halogen.HTML.Properties (class_)
import Halogen.VDom.Driver (runUI)
import UntypedLambda (eval, display, standardLibs)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI
      ( mkComponent
          { initialState: const {}
          , render:
            const
              $ main_
                  [ h1 [] [ text "REPL for the untyped lambda calculus" ]
                  , section_
                      [ h2 [] [ text "Environment:" ]
                      , ul [ class_ $ ClassName "unstyled" ] $ standardLibs
                          # map \(Tuple name value) ->
                              li_ [ text $ name <> " = " <> display value ]
                      , section_
                          [ h3 [] [ text "Input:" ]
                          , div [ class_ $ ClassName "indent" ] [ text "succ zero" ]
                          ]
                      , section_ $ eval standardLibs "succ zero"
                          # case _ of
                              Left err ->
                                [ h3 [] [ text "Error:" ]
                                , div [ class_ $ ClassName "indent" ] [ text $ show err ]
                                ]
                              Right (Tuple value env) ->
                                [ h3 [] [ text "Result:" ]
                                , div [ class_ $ ClassName "indent" ] [ text $ display value ]
                                ]
                      ]
                  ]
          , eval: mkEval $ defaultEval { handleAction = \(Identity a) -> pure a }
          }
      )
      unit
      body
