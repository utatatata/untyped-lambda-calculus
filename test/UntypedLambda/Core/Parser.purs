module Test.UntypedLambda.Core.Parser
  ( testTerm
  ) where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)
import UntypedLambda.Core.Term (Term(..))
import UntypedLambda.Core.Parser (term)

testTerm :: Spec Unit
testTerm =
  describe "UntypedLambda.Parser" do
    describe "term" do
      testValid "x" $ Variable "x"
      testValid "zero?" $ Variable "zero?"
      testValid "(λx.x)" $ LambdaAbstraction "x" (Variable "x")
      testValid "(  λ  x  y    z . ( x  y  z   ))" $ LambdaAbstraction "x" $ LambdaAbstraction "y" $ LambdaAbstraction "z" $ Application (Application (Variable "x") (Variable "y")) (Variable "z")
      testValid "(\\x.x)" $ LambdaAbstraction "x" (Variable "x")
      testValid "(\\f.\\x.(f x))" $ LambdaAbstraction "f" $ LambdaAbstraction "x" $ Application (Variable "f") (Variable "x")
      testValid "(succ zero)" $ Application (Variable "succ") (Variable "zero")
      testValid "(x y z)" $ Application (Application (Variable "x") (Variable "y")) (Variable "z")
      testValid "(  succ   one   )" $ Application (Variable "succ") (Variable "one")
      testValid "(  a   b c    d   e)" $ Application (Application (Application (Application (Variable "a") (Variable "b")) (Variable "c")) (Variable "d")) (Variable "e")
      testValid "x y z" $ Application (Application (Variable "x") (Variable "y")) (Variable "z")
      testValid "λx.x y" $ LambdaAbstraction "x" $ Application (Variable "x") (Variable "y")
      testValid "λn f x.f (n f x)" $ LambdaAbstraction "n" $ LambdaAbstraction "f" $ LambdaAbstraction "x" $ Application (Variable "f") $ Application (Application (Variable "n") (Variable "f")) (Variable "x")
      testValid "λ   m  n f x  .m f ( n f x  )" $ LambdaAbstraction "m" $ LambdaAbstraction "n" $ LambdaAbstraction "f" $ LambdaAbstraction "x" $ Application (Application (Variable "m") (Variable "f")) $ Application (Application (Variable "n") (Variable "f")) (Variable "x")
      testValid "x (λy.y) z" $ Application (Application (Variable "x") $ LambdaAbstraction "y" $ Variable "y") $ Variable "z"
      testValid "x λy.y z" $ Application (Variable "x") $ LambdaAbstraction "y" $ Application (Variable "y") $ Variable "z"
      testValid "x\n" $ Variable "x"
      testValid "x\ny" $ Application (Variable "x") $ Variable "y"
  where
  testValid code result =
    it code do
      runParser code term `shouldEqual` pure result
