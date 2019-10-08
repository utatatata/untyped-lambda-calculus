module Test.UntypedLambda.Repl
  ( testRepl
  ) where

import Prelude
import Data.Array (cons)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import UntypedLambda.Core (Expression(..), VApplication(..), Value(..), standardLibs)
import UntypedLambda.Repl (repl)

testRepl :: Spec Unit
testRepl =
  describe "UntypedLambda.Repl" do
    describe "repl" do
      it "λx.x" do
        repl standardLibs "λx.x" `shouldEqual` (pure $ Tuple (VLambdaAbstraction "x" (VVariable "x")) standardLibs)
      it "a=a" do
        repl standardLibs "a=a" `shouldEqual` (pure $ Tuple (VVariable "a") $ cons (Tuple "a" (Variable "a")) standardLibs)
      it "foo = λn f x.f (n f x)" do
        let
          value = VLambdaAbstraction "n" $ VLambdaAbstraction "f" $ VLambdaAbstraction "x" $ VApplication $ VIdentApp "f" $ VApplication $ VAppApp (VIdentApp "n" (VVariable "f")) (VVariable "x")

          expr = LambdaAbstraction "n" $ LambdaAbstraction "f" $ LambdaAbstraction "x" $ Application (Variable "f") $ Application (Application (Variable "n") (Variable "f")) (Variable "x")
        repl standardLibs "foo = λn f x.f (n f x)" `shouldEqual` (pure $ Tuple value $ cons (Tuple "foo" expr) standardLibs)
