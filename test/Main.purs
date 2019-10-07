module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Core (testAlphaConversion, testBetaReduction, testCallByValue, testEtaConversion, testFreeVariables)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ]
    $ do
        testFreeVariables
        testAlphaConversion
        testBetaReduction
        testEtaConversion
        testCallByValue
