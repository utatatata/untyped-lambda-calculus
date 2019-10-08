module Test.UntypedLambda.Parser
  ( testExpression
  ) where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)
import UntypedLambda.Core (Expression(..))
import UntypedLambda.Parser (expression)

testExpression :: Spec Unit
testExpression =
  describe "UntypedLambda.Parser" do
    describe "expression" do
      testValid "x" $ Variable "x"
      testValid "zero?" $ Variable "zero?"
      testValid "(λx.x)" $ LambdaAbstraction "x" (Variable "x")
      testValid "(  λ  x  y    z . ( x  y  z   ))" $ LambdaAbstraction "x" $ LambdaAbstraction "y" $ LambdaAbstraction "z" $ Application (Application (Variable "x") (Variable "y")) (Variable "z")
      testValid "(\\x.x)" $ LambdaAbstraction "x" (Variable "x")
      testValid "(succ zero)" $ Application (Variable "succ") (Variable "zero")
      testValid "(x y z)" $ Application (Application (Variable "x") (Variable "y")) (Variable "z")
      testValid "(  succ   one   )" $ Application (Variable "succ") (Variable "one")
      testValid "(  a   b c    d   e)" $ Application (Application (Application (Application (Variable "a") (Variable "b")) (Variable "c")) (Variable "d")) (Variable "e")
  where
  testValid code result =
    it code do
      runParser code expression `shouldEqual` pure result
