module Test.UntypedLambda.Repl
  ( testRepl
  ) where

import Prelude
import Data.Array (cons)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import UntypedLambda.Core (Expression(..), VApplication(..), Value(..), standardLibs)
import UntypedLambda.Repl (eval)

testRepl :: Spec Unit
testRepl =
  describe "UntypedLambda.Repl" do
    describe "eval" do
      it "λx.x" do
        eval standardLibs "λx.x" `shouldEqual` (pure $ Tuple (VLambdaAbstraction "x" (VVariable "x")) standardLibs)
      it "a=a" do
        eval standardLibs "a=a" `shouldEqual` (pure $ Tuple (VVariable "a") $ cons (Tuple "a" (VVariable "a")) standardLibs)
      it "foo = λn f x.f (n f x)" do
        let
          value = VLambdaAbstraction "n" $ VLambdaAbstraction "f" $ VLambdaAbstraction "x" $ VApplication $ VIdentApp "f" $ VApplication $ VAppApp (VIdentApp "n" (VVariable "f")) (VVariable "x")
        eval standardLibs "foo = λn f x.f (n f x)" `shouldEqual` (pure $ Tuple value $ cons (Tuple "foo" value) standardLibs)
