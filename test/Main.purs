module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.UntypedLambda.Core.Evaluator (testAlphaConversion, testBetaReduction, testCallByValue, testEtaConversion, testFreeVariables)
import Test.UntypedLambda.Core.Parser (testTerm)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ]
    $ do
        testFreeVariables
        testAlphaConversion
        testBetaReduction
        testEtaConversion
        testCallByValue
        testTerm
