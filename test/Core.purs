module Test.Core
  ( testFreeVariables
  , testAlphaConversion
  , testBetaReduction
  , testEtaConversion
  , testCallByValue
  ) where

import Prelude
import UntypedLambda.Core (Expression(..), Substitution(..), VApplication(..), Value(..), alphaConversion, betaReduction, callByValue, etaConversion, freeVariables, standardLibs, withEnvironment)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testFreeVariables :: Spec Unit
testFreeVariables =
  describe "freeVariables" do
    it "FV(λx.y y_0) = { y, y_0 }" do
      freeVariables (LambdaAbstraction "x" (Application (Variable "y") (Variable "y_0"))) `shouldEqual` [ "y", "y_0" ]

testAlphaConversion :: Spec Unit
testAlphaConversion =
  describe "alphaConversion: M[x := N]" do
    describe "x[x := N] ≡ N" do
      describe "N = <variable>" do
        it "x[x := x] ≡ x" do
          alphaConversion (Substitution "x" (Variable "x")) (Variable "x") `shouldEqual` Variable "x"
        it "x[x := y] ≡ y" do
          alphaConversion (Substitution "x" (Variable "y")) (Variable "x") `shouldEqual` Variable "y"
      describe "N = λ<variable>.<variable>" do
        it "x[x := λx.x] ≡ λx.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "x"))) (Variable "x") `shouldEqual` LambdaAbstraction "x" (Variable "x")
        it "x[x := λx.y] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "y"))) (Variable "x") `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "x[x := λy.x] ≡ λy.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "x"))) (Variable "x") `shouldEqual` LambdaAbstraction "y" (Variable "x")
        it "x[x := λy.z] ≡ λy.z" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "z"))) (Variable "x") `shouldEqual` LambdaAbstraction "y" (Variable "z")
      describe "N = (<variable> <variable>)" do
        it "x[x := x x] ≡ x x" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "x"))) (Variable "x") `shouldEqual` Application (Variable "x") (Variable "x")
        it "x[x := x y] ≡ x y" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "y"))) (Variable "x") `shouldEqual` Application (Variable "x") (Variable "y")
        it "x[x := y x] ≡ y x" do
          alphaConversion (Substitution "x" (Application (Variable "y") (Variable "x"))) (Variable "x") `shouldEqual` Application (Variable "y") (Variable "x")
        it "x[x := y z] ≡ y z" do
          alphaConversion (Substitution "x" (Application (Variable "y") (Variable "z"))) (Variable "x") `shouldEqual` Application (Variable "y") (Variable "z")
    describe "y[x := N] ≡ y, if x ≠ y" do
      describe "N = <variable>" do
        it "y[x := x] ≡ y" do
          alphaConversion (Substitution "x" (Variable "x")) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := y] ≡ y" do
          alphaConversion (Substitution "x" (Variable "y")) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := z] ≡ y" do
          alphaConversion (Substitution "x" (Variable "z")) (Variable "y") `shouldEqual` Variable "y"
      describe "N = λ<variable>.<variable>" do
        it "y[x := λx.x] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "x"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λx.y] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "y"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λy.x] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "x"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λy.y] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "y"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λx.z] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "z"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λy.z] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "z"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λz.x] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "x"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λz.y] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "y"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := λz.w] ≡ y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "w"))) (Variable "y") `shouldEqual` Variable "y"
      describe "N = (<variable> <variable>)" do
        it "y[x := x x] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "x"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := x y] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "y"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := y x] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "y") (Variable "x"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := y y] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "y") (Variable "y"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := x z] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "z"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := y z] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "y") (Variable "z"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := z x] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "z") (Variable "x"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := z y] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "z") (Variable "y"))) (Variable "y") `shouldEqual` Variable "y"
        it "y[x := z w] ≡ y" do
          alphaConversion (Substitution "x" (Application (Variable "z") (Variable "w"))) (Variable "y") `shouldEqual` Variable "y"
    describe "(λx.M)[x := N] ≡ λx.M" do
      describe "M = <variable>, N = <variable>" do
        -- M = x
        it "(λx.x)[x := x] ≡ λx.y" do
          alphaConversion (Substitution "x" (Variable "x")) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        it "(λx.x)[x := y] ≡ λx.x" do
          alphaConversion (Substitution "x" (Variable "y")) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        -- M = y
        it "(λx.y)[x := x] ≡ λx.y" do
          alphaConversion (Substitution "x" (Variable "x")) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := y] ≡ λx.y" do
          alphaConversion (Substitution "x" (Variable "y")) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := z] ≡ λx.y" do
          alphaConversion (Substitution "x" (Variable "z")) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
      describe "M = <variable>, N = λ<variable>.<variable>" do
        -- M = x
        it "(λx.x)[x := λx.x] ≡ λx.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "x"))) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        it "(λx.x)[x := λx.y] ≡ λx.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "y"))) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        it "(λx.x)[x := λy.x] ≡ λx.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "x"))) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        it "(λx.x)[x := λy.y] ≡ λx.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "y"))) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        -- M = y
        it "(λx.y)[x := λx.x] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "x"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λx.y] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "y"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λy.x] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "x"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λy.y] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "y"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λz.x] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "x"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λz.y] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "y"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λx.z] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "z"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λy.z] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "z"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.y)[x := λz.w] ≡ λx.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "w"))) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
    describe "(λy.M)[x := N] ≡ λy.(M[x := N]), if x ≠ y, provided y ∉ FV(N)" do
      describe "M = <variable>, N = <variable>" do
        -- M = x
        it "(λy.x)[x := x] ≡ λy.(x[x := x]) ≡ λy.x" do
          alphaConversion (Substitution "x" (Variable "x")) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Variable "x")
        it "(λy.x)[x := z] ≡ λy.(x[x := z]) ≡ λy.z" do
          alphaConversion (Substitution "x" (Variable "z")) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Variable "z")
        -- M = y
        it "(λy.y)[x := x] ≡ λy.(y[x := x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Variable "x")) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := z] ≡ λy.(y[x := z]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Variable "z")) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
      describe "M = <variable>, N = λ<variable>.<variable>" do
        -- M = x
        it "(λy.x)[x := λx.x] ≡ λy.(x[x := λx.x]) ≡ λy x.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "x"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "x" (Variable "x"))
        it "(λy.x)[x := λy.x] ≡ λy.(x[x := λy.x]) ≡ λy y.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "x"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "y" (Variable "x"))
        it "(λy.x)[x := λy.y] ≡ λy.(x[x := λy.y]) ≡ λy y.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "y"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "y" (Variable "y"))
        it "(λy.x)[x := λx.z] ≡ λy.(x[x := λx.z]) ≡ λy x.z" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "z"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "x" (Variable "z"))
        it "(λy.x)[x := λy.z] ≡ λy.(x[x := λy.z]) ≡ λy y.z" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "z"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "y" (Variable "z"))
        it "(λy.x)[x := λz.x] ≡ λy.(x[x := λz.x]) ≡ λy z.x" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "x"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "z" (Variable "x"))
        it "(λy.x)[x := λz.z] ≡ λy.(x[x := λz.z]) ≡ λy z.z" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "z"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "z" (Variable "z"))
        -- M = y
        it "(λy.y)[x := λx.x] ≡ λy.(y[x := λx.x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "x"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λy.x] ≡ λy.(y[x := λy.x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "x"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λy.y] ≡ λy.(y[x := λy.y]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "y"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λx.z] ≡ λy.(y[x := λx.z]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "z"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λy.z] ≡ λy.(y[x := λy.z]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "y" (Variable "z"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λz.x] ≡ λy.(y[x := λz.x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "x"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λz.w] ≡ λy.(y[x := λz.w]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "w"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
      describe "M = <variable>, N = (<variable> <variable>)" do
        -- M = x
        it "(λy.x)[x := x x] ≡ λy.(x[x := x x]) ≡ λy.x x" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "x"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Variable "x") (Variable "x"))
        it "(λy.x)[x := x z] ≡ λy.(x[x := x x]) ≡ λy.x z" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "z"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Variable "x") (Variable "z"))
        it "(λy.x)[x := z x] ≡ λy.(x[x := x x]) ≡ λy.z x" do
          alphaConversion (Substitution "x" (Application (Variable "z") (Variable "x"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Variable "z") (Variable "x"))
        it "(λy.x)[x := z w] ≡ λy.(x[x := x x]) ≡ λy.z w" do
          alphaConversion (Substitution "x" (Application (Variable "z") (Variable "w"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Variable "z") (Variable "w"))
        -- M = y
        it "(λy.y)[x := x x] ≡ λy.(y[x := x x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "x"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := x z] ≡ λy.(y[x := x x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Variable "x") (Variable "z"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := z x] ≡ λy.(y[x := x x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Variable "z") (Variable "x"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := z w] ≡ λy.(y[x := x x]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Variable "z") (Variable "w"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
    describe "(λy.M)[x := N] ≡ λy.(M[x := N[y := y_0]]), if x ≠ y, provided y ∈ FV(N), provided y_0 ∉ FV(N)" do
      describe "M = <variable>, N = <variable>" do
        -- M = x
        it "(λy.x)[x := y] ≡ λy.(x[x := y[y := y_0]]) ≡ λy.y_0" do
          alphaConversion (Substitution "x" (Variable "y")) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Variable "y_0")
        -- M = y
        it "(λy.y)[x := y] ≡ λy.(y[x := y[y := y_0]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Variable "y")) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
      describe "M = <variable>, N = λ<variable>.<variable>" do
        -- M = x
        it "(λy.x)[x := λx.y] ≡ λy.(x[x:= (λx.y)[y := y_0]]) ≡ λy x.y_0" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "y"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "x" (Variable "y_0"))
        it "(λy.x)[x := λz.y] ≡ λy.(x[x:= (λz.y)[y := y_0]]) ≡ λy z.y_0" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "y"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "z" (Variable "y_0"))
        -- M = y
        it "(λy.y)[x := λx.y] ≡ λy.(y[x := (λx.y)[y := y_0]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Variable "y"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λz.y] ≡ λy.(y[x := (λz.y)[y := y_0]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Variable "y"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
    describe "(λy.M)[x := N] ≡ λy.(M[x := N[y := y_1]]), if x ≠ y, provided y ∈ FV(N), provided y_0 ∈ FV(N), provided y_1 ∉ FV(N)" do
      describe "M = <variable>, N = λ<variable>.(<variable> <variable>)" do
        -- M = x
        it "(λy.x)[x := λx.y y_0] ≡ λy.(x[x := (λx.y y_0)[y := y_1]]) ≡ λy x.y_1 y_0" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Application (Variable "y") (Variable "y_0")))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "x" (Application (Variable "y_1") (Variable "y_0")))
        it "(λy.x)[x := λx.y_0 y] ≡ λy.(x[x := (λx.y_0 y)[y := y_1]]) ≡ λy x.y_0 y_1" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Application (Variable "y_0") (Variable "y")))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "x" (Application (Variable "y_0") (Variable "y_1")))
        it "(λy.x)[x := λz.y y_0] ≡ λy.(x[x := (λz.y y_0)[y := y_1]]) ≡ λy z.y_1 y_0" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Application (Variable "y") (Variable "y_0")))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "z" (Application (Variable "y_1") (Variable "y_0")))
        it "(λy.x)[x := λz.y_0 y] ≡ λy.(x[x := (λz.y_0 y)[y := y_1]]) ≡ λy z.y_0 y_1" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Application (Variable "y_0") (Variable "y")))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (LambdaAbstraction "z" (Application (Variable "y_0") (Variable "y_1")))
        -- M = y
        it "(λy.y)[x := λx.y y_0] ≡ λy.(x[x := (λx.y y_0)[y := y_1]]) ≡ λy y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Application (Variable "y") (Variable "y_0")))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λx.y_0 y] ≡ λy.(x[x := (λx.y_0 y)[y := y_1]]) ≡ λy y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "x" (Application (Variable "y_0") (Variable "y")))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λz.y y_0] ≡ λy.(x[x := (λz.y y_0)[y := y_1]]) ≡ λy y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Application (Variable "y") (Variable "y_0")))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := λz.y_0 y] ≡ λy.(x[x := (λz.y_0 y)[y := y_1]]) ≡ λy y" do
          alphaConversion (Substitution "x" (LambdaAbstraction "z" (Application (Variable "y_0") (Variable "y")))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
      describe "M = <variable>, N = (<variable> (<variable> <variable>))" do
        -- M = x
        it "(λy.x)[x := x y y_0] ≡　λy.(x[x := (x y y_0)[y := y_1]]) ≡ λy.x y_1 y_0" do
          alphaConversion (Substitution "x" (Application (Application (Variable "x") (Variable "y")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "x") (Variable "y_1")) (Variable "y_0"))
        it "(λy.x)[x := x y_0 y] ≡　λy.(x[x := (x y_0 y)[y := y_1]]) ≡ λy.x y_0 y_1" do
          alphaConversion (Substitution "x" (Application (Application (Variable "x") (Variable "y_0")) (Variable "y"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "x") (Variable "y_0")) (Variable "y_1"))
        it "(λy.x)[x := z y y_0] ≡　λy.(x[x := (z y y_0)[y := y_1]]) ≡ λy.z y_1 y_0" do
          alphaConversion (Substitution "x" (Application (Application (Variable "z") (Variable "y")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "z") (Variable "y_1")) (Variable "y_0"))
        it "(λy.x)[x := z y_0 y] ≡　λy.(x[x := (z y_0 y)[y := y_1]]) ≡ λy.z y_0 y_1" do
          alphaConversion (Substitution "x" (Application (Application (Variable "z") (Variable "y_0")) (Variable "y"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "z") (Variable "y_0")) (Variable "y_1"))
        it "(λy.x)[x := y x y_0] ≡　λy.(x[x := (y x y_0)[y := y_1]]) ≡ λy.y_1 x y_0" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "x")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_1") (Variable "x")) (Variable "y_0"))
        it "(λy.x)[x := y_0 x y] ≡　λy.(x[x := (y_0 x y)[y := y_1]]) ≡ λy.y_0 x y_1" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "x")) (Variable "y"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_0") (Variable "x")) (Variable "y_1"))
        it "(λy.x)[x := y z y_0] ≡　λy.(x[x := (y z y_0)[y := y_1]]) ≡ λy.y_1 z y_0" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "z")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_1") (Variable "z")) (Variable "y_0"))
        it "(λy.x)[x := y_0 z y] ≡　λy.(x[x := (y_0 z y)[y := y_1]]) ≡ λy.y_0 z y_1" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "z")) (Variable "y"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_0") (Variable "z")) (Variable "y_1"))
        it "(λy.x)[x := y y_0 x] ≡　λy.(x[x := (y y_0 x)[y := y_1]]) ≡ λy.y_1 y_0 x" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "y_0")) (Variable "x"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_1") (Variable "y_0")) (Variable "x"))
        it "(λy.x)[x := y_0 y x] ≡　λy.(x[x := (y_0 y x)[y := y_1]]) ≡ λy.y_0 y_1 x" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "y")) (Variable "x"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_0") (Variable "y_1")) (Variable "x"))
        it "(λy.x)[x := y y_0 z] ≡　λy.(x[x := (y y_0 z)[y := y_1]]) ≡ λy.y_1 y_0 z" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "y_0")) (Variable "z"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_1") (Variable "y_0")) (Variable "z"))
        it "(λy.x)[x := y_0 y z] ≡　λy.(x[x := (y_0 y z)[y := y_1]]) ≡ λy.y_0 y_1 z" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "y")) (Variable "z"))) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Application (Application (Variable "y_0") (Variable "y_1")) (Variable "z"))
        -- M = y
        it "(λy.y)[x := x y y_0] ≡　λy.(x[x := (x y y_0)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "x") (Variable "y")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := x y_0 y] ≡　λy.(x[x := (x y_0 y)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "x") (Variable "y_0")) (Variable "y"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := z y y_0] ≡　λy.(x[x := (z y y_0)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "z") (Variable "y")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := z y_0 y] ≡　λy.(x[x := (z y_0 y)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "z") (Variable "y_0")) (Variable "y"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y x y_0] ≡　λy.(x[x := (y x y_0)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "x")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y_0 x y] ≡　λy.(x[x := (y_0 x y)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "x")) (Variable "y"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y z y_0] ≡　λy.(x[x := (y z y_0)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "z")) (Variable "y_0"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y_0 z y] ≡　λy.(x[x := (y_0 z y)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "z")) (Variable "y"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y y_0 x] ≡　λy.(x[x := (y y_0 x)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "y_0")) (Variable "x"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y_0 y x] ≡　λy.(x[x := (y_0 y x)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "y")) (Variable "x"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y y_0 z] ≡　λy.(x[x := (y y_0 z)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y") (Variable "y_0")) (Variable "z"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λy.y)[x := y_0 y z] ≡　λy.(x[x := (y_0 y z)[y := y_1]]) ≡ λy.y" do
          alphaConversion (Substitution "x" (Application (Application (Variable "y_0") (Variable "y")) (Variable "z"))) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")

testBetaReduction :: Spec Unit
testBetaReduction =
  describe "betaReduction" do
    describe "(λx.M) N ≡ M[x := N]" do
      describe "M = <variable>, N = <variable>" do
        -- M = x
        it "(λx.x) x ≡ x[x := x] ≡ x" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (Variable "x") `shouldEqual` Variable "x"
        it "(λx.x) y ≡ x[x := y] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (Variable "y") `shouldEqual` Variable "y"
        -- M = y
        it "(λx.y) x ≡ y[x := x] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (Variable "x") `shouldEqual` Variable "y"
        it "(λx.y) y ≡ y[x := y] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (Variable "x") `shouldEqual` Variable "y"
        it "(λx.y) z ≡ y[x := z] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (Variable "z") `shouldEqual` Variable "y"
      describe "M = <variable>, N = λ<variable>.<variable>" do
        -- M = x
        it "(λx.x) λx.x ≡ x[x := λx.x] ≡ λx.x" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        it "(λx.x) λx.y ≡ x[x := λx.y] ≡ λx.y" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "(λx.x) λy.x ≡ x[x := λy.x] ≡ λy.x" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Variable "x")
        it "(λx.x) λy.y ≡ x[x := λy.y] ≡ λy.y" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` LambdaAbstraction "y" (Variable "y")
        it "(λx.x) λy.z ≡ x[x := λy.z] ≡ λy.z" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (LambdaAbstraction "y" (Variable "z")) `shouldEqual` LambdaAbstraction "y" (Variable "z")
        -- M = y
        it "(λx.y) λx.x ≡ y[x := λx.x] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "x" (Variable "x")) `shouldEqual` Variable "y"
        it "(λx.y) λx.y ≡ y[x := λx.y] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "x" (Variable "y")) `shouldEqual` Variable "y"
        it "(λx.y) λy.x ≡ y[x := λy.x] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "y" (Variable "x")) `shouldEqual` Variable "y"
        it "(λx.y) λy.y ≡ y[x := λy.y] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "y" (Variable "y")) `shouldEqual` Variable "y"
        it "(λx.y) λx.z ≡ y[x := λx.z] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "x" (Variable "z")) `shouldEqual` Variable "y"
        it "(λx.y) λy.z ≡ y[x := λy.z] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "y" (Variable "z")) `shouldEqual` Variable "y"
        it "(λx.y) λz.x ≡ y[x := λz.x] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "z" (Variable "x")) `shouldEqual` Variable "y"
        it "(λx.y) λz.y ≡ y[x := λz.y] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (LambdaAbstraction "z" (Variable "y")) `shouldEqual` Variable "y"
      describe "M = <variable>, N = (<variable> <variable>)" do
        -- M = x
        it "(λx.x) (x x) ≡ x[x := x x] ≡ x x" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (Application (Variable "x") (Variable "x")) `shouldEqual` Application (Variable "x") (Variable "x")
        it "(λx.x) (x y) ≡ x[x := x y] ≡ x y" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (Application (Variable "x") (Variable "y")) `shouldEqual` Application (Variable "x") (Variable "y")
        it "(λx.x) (y x) ≡ x[x := y x] ≡ y x" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (Application (Variable "y") (Variable "x")) `shouldEqual` Application (Variable "y") (Variable "x")
        it "(λx.x) (y z) ≡ x[x := y z] ≡ y z" do
          betaReduction (LambdaAbstraction "x" (Variable "x")) (Application (Variable "y") (Variable "z")) `shouldEqual` Application (Variable "y") (Variable "z")
        -- M = y
        it "(λx.y) (x x) ≡ y[x := x x] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (Application (Variable "x") (Variable "x")) `shouldEqual` Variable "y"
        it "(λx.y) (x y) ≡ y[x := x y] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (Application (Variable "x") (Variable "y")) `shouldEqual` Variable "y"
        it "(λx.y) (y x) ≡ y[x := y x] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (Application (Variable "y") (Variable "x")) `shouldEqual` Variable "y"
        it "(λx.y) (y z) ≡ y[x := y z] ≡ y" do
          betaReduction (LambdaAbstraction "x" (Variable "y")) (Application (Variable "y") (Variable "z")) `shouldEqual` Variable "y"
    describe "x M ≡ x M" do
      describe "M = <variable>" do
        it "x x ≡ x x" do
          shouldSame (Variable "x") (Variable "x")
        it "x y ≡ x y" do
          shouldSame (Variable "x") (Variable "y")
      describe "M = λ<variable>.<variable>" do
        it "x λx.x ≡ x λx.x" do
          shouldSame (Variable "x") (LambdaAbstraction "x" (Variable "x"))
        it "x λx.y ≡ x λx.y" do
          shouldSame (Variable "x") (LambdaAbstraction "x" (Variable "y"))
        it "x λy.x ≡ x λy.x" do
          shouldSame (Variable "x") (LambdaAbstraction "y" (Variable "x"))
        it "x λy.z ≡ x λy.z" do
          shouldSame (Variable "x") (LambdaAbstraction "y" (Variable "z"))
      describe "M = (<variable> <variable>)" do
        it "x (x x) ≡ x (x x)" do
          shouldSame (Variable "x") (Application (Variable "x") (Variable "x"))
        it "x (x y) ≡ x (x y)" do
          shouldSame (Variable "x") (Application (Variable "x") (Variable "y"))
        it "x (y x) ≡ x (y x)" do
          shouldSame (Variable "x") (Application (Variable "y") (Variable "x"))
        it "x (y z) ≡ x (y z)" do
          shouldSame (Variable "x") (Application (Variable "y") (Variable "z"))
    describe "(M1 M2) N ≡ (M1 M2) N" do
      describe "M1 = <variable>, M2 = <variable>, N = <variable>" do
        it "(x x) x ≡ (x x) x" do
          shouldSame (Application (Variable "x") (Variable "x")) (Variable "x")
        it "(x y) x ≡ (x y) x" do
          shouldSame (Application (Variable "x") (Variable "y")) (Variable "x")
        it "(y x) x ≡ (y x) x" do
          shouldSame (Application (Variable "y") (Variable "x")) (Variable "x")
        it "(y z) x ≡ (y z) x" do
          shouldSame (Application (Variable "y") (Variable "z")) (Variable "x")
      describe "M1 = <variable>, M2 = <variable>, N = λ<variable>.<variable>" do
        -- (x x) N
        it "(x x) λx.x ≡ (x x) λx.x" do
          shouldSame (Application (Variable "x") (Variable "x")) (LambdaAbstraction "x" (Variable "x"))
        it "(x x) λx.y ≡ (x x) λx.y" do
          shouldSame (Application (Variable "x") (Variable "x")) (LambdaAbstraction "x" (Variable "y"))
        it "(x x) λy.x ≡ (x x) λy.x" do
          shouldSame (Application (Variable "x") (Variable "x")) (LambdaAbstraction "y" (Variable "x"))
        it "(x x) λy.z ≡ (x x) λy.z" do
          shouldSame (Application (Variable "x") (Variable "x")) (LambdaAbstraction "y" (Variable "z"))
        -- (x y) N
        it "(x y) λx.x ≡ (x y) λx.x" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "x" (Variable "x"))
        it "(x y) λx.y ≡ (x y) λx.y" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "x" (Variable "y"))
        it "(x y) λy.x ≡ (x y) λy.x" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "y" (Variable "x"))
        it "(x y) λy.y ≡ (x y) λy.y" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "y" (Variable "y"))
        it "(x y) λx.z ≡ (x y) λx.z" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "x" (Variable "z"))
        it "(x y) λy.z ≡ (x y) λy.z" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "y" (Variable "z"))
        it "(x y) λz.x ≡ (x y) λz.x" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "z" (Variable "x"))
        it "(x y) λz.y ≡ (x y) λz.y" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "z" (Variable "y"))
        it "(x y) λz.w ≡ (x y) λz.w" do
          shouldSame (Application (Variable "x") (Variable "y")) (LambdaAbstraction "z" (Variable "w"))
      describe "M1 = <variable>, M2 = <variable>, N = (<variable> <variable>)" do
        -- (x x) (N1 N2)
        it "(x x) (x x) ≡ (x x) (x x)" do
          shouldSame (Application (Variable "x") (Variable "x")) (Application (Variable "x") (Variable "x"))
        it "(x x) (x y) ≡ (x x) (x y)" do
          shouldSame (Application (Variable "x") (Variable "x")) (Application (Variable "x") (Variable "y"))
        it "(x x) (y x) ≡ (x x) (y x)" do
          shouldSame (Application (Variable "x") (Variable "x")) (Application (Variable "y") (Variable "x"))
        it "(x x) (y z) ≡ (x x) (y z)" do
          shouldSame (Application (Variable "x") (Variable "x")) (Application (Variable "y") (Variable "y"))
        -- (x y) (N1 N2)
        it "(x y) (x x) ≡ (x y) (x x)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "x") (Variable "x"))
        it "(x y) (x y) ≡ (x y) (x y)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "x") (Variable "y"))
        it "(x y) (y x) ≡ (x y) (y x)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "y") (Variable "x"))
        it "(x y) (y y) ≡ (x y) (y y)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "y") (Variable "y"))
        it "(x y) (x z) ≡ (x y) (x z)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "x") (Variable "z"))
        it "(x y) (y z) ≡ (x y) (y z)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "y") (Variable "z"))
        it "(x y) (z x) ≡ (x y) (z x)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "z") (Variable "x"))
        it "(x y) (z y) ≡ (x y) (z y)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "z") (Variable "y"))
        it "(x y) (z w) ≡ (x y) (z w)" do
          shouldSame (Application (Variable "x") (Variable "y")) (Application (Variable "z") (Variable "w"))
  where
  shouldSame expr arg = betaReduction expr arg `shouldEqual` Application expr arg

testEtaConversion :: Spec Unit
testEtaConversion =
  describe "etaConversion" do
    describe "λx.(M x), provided x ∉ FV(M)" do
      describe "M = <variable>" do
        it "λx.y x ≡ y" do
          etaConversion "x" (Application (Variable "y") (Variable "x")) `shouldEqual` Variable "y"
      describe "M = λ<variable>.<variable>" do
        it "λx.(λx.x) x ≡ λx.x" do
          etaConversion "x" (Application (LambdaAbstraction "x" (Variable "x")) (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "x")
        it "λx.(λx.y) x ≡ λx.y" do
          etaConversion "x" (Application (LambdaAbstraction "x" (Variable "y")) (Variable "x")) `shouldEqual` LambdaAbstraction "x" (Variable "y")
        it "λx.(λy.z) x ≡ λy.z" do
          etaConversion "x" (Application (LambdaAbstraction "y" (Variable "z")) (Variable "x")) `shouldEqual` LambdaAbstraction "y" (Variable "z")
      describe "M = (<variable> <variable>)" do
        it "λx.(y y) x ≡ y y" do
          etaConversion "x" (Application (Application (Variable "y") (Variable "y")) (Variable "x")) `shouldEqual` Application (Variable "y") (Variable "y")
        it "λx.(y z) x ≡ y z" do
          etaConversion "x" (Application (Application (Variable "y") (Variable "z")) (Variable "x")) `shouldEqual` Application (Variable "y") (Variable "z")
    describe "λx.(M x), provided x ∈ FV(M)" do
      describe "M = <variable>" do
        it "λx.x x ≡ λx.x x" do
          shouldSame "x" (Application (Variable "x") (Variable "x"))
      describe "M = λ<variable>.<variable>" do
        it "λx.(λy.x) x ≡ λx.(λy.x) x" do
          shouldSame "x" (Application (LambdaAbstraction "y" (Variable "x")) (Variable "x"))
      describe "M = (<variable> <variable>)" do
        it "λx.(x x) x ≡ λx.(x x) x" do
          shouldSame "x" (Application (Application (Variable "x") (Variable "x")) (Variable "x"))
        it "λx.(x y) x ≡ λx.(x y) x" do
          shouldSame "x" (Application (Application (Variable "x") (Variable "y")) (Variable "x"))
        it "λx.(y x) x ≡ λx.(y x) x" do
          shouldSame "x" (Application (Application (Variable "y") (Variable "x")) (Variable "x"))
    describe "λx.(M y), if x ≠ y" do
      describe "M = <variable>" do
        it "λx.(x y) ≡ λx.(x y)" do
          shouldSame "x" (Application (Variable "x") (Variable "y"))
        it "λx.(y y) ≡ λx.(y y)" do
          shouldSame "x" (Application (Variable "y") (Variable "y"))
        it "λx.(z y) ≡ λx.(z y)" do
          shouldSame "x" (Application (Variable "z") (Variable "y"))
      describe "M = λ<variable>.<variable>" do
        it "λx.(λx.x) y" do
          shouldSame "x" (Application (LambdaAbstraction "x" (Variable "x")) (Variable "y"))
        it "λx.(λx.y) y" do
          shouldSame "x" (Application (LambdaAbstraction "x" (Variable "y")) (Variable "y"))
        it "λx.(λy.x) y" do
          shouldSame "x" (Application (LambdaAbstraction "y" (Variable "x")) (Variable "y"))
        it "λx.(λy.y) y" do
          shouldSame "x" (Application (LambdaAbstraction "y" (Variable "y")) (Variable "y"))
        it "λx.(λx.z) y" do
          shouldSame "x" (Application (LambdaAbstraction "x" (Variable "z")) (Variable "y"))
        it "λx.(λy.z) y" do
          shouldSame "x" (Application (LambdaAbstraction "y" (Variable "z")) (Variable "y"))
        it "λx.(λz.x) y" do
          shouldSame "x" (Application (LambdaAbstraction "z" (Variable "x")) (Variable "y"))
        it "λx.(λz.y) y" do
          shouldSame "x" (Application (LambdaAbstraction "z" (Variable "y")) (Variable "y"))
        it "λx.(λz.w) y" do
          shouldSame "x" (Application (LambdaAbstraction "z" (Variable "w")) (Variable "y"))
      describe "M = (<variable> <variable>)" do
        it "λx.(x x) y" do
          shouldSame "x" (Application (Application (Variable "x") (Variable "x")) (Variable "y"))
        it "λx.(x y) y" do
          shouldSame "x" (Application (Application (Variable "x") (Variable "y")) (Variable "y"))
        it "λx.(y x) y" do
          shouldSame "x" (Application (Application (Variable "y") (Variable "x")) (Variable "y"))
        it "λx.(y y) y" do
          shouldSame "x" (Application (Application (Variable "y") (Variable "y")) (Variable "y"))
        it "λx.(x z) y" do
          shouldSame "x" (Application (Application (Variable "x") (Variable "z")) (Variable "y"))
        it "λx.(y z) y" do
          shouldSame "x" (Application (Application (Variable "y") (Variable "z")) (Variable "y"))
        it "λx.(z x) y" do
          shouldSame "x" (Application (Application (Variable "z") (Variable "x")) (Variable "y"))
        it "λx.(z y) y" do
          shouldSame "x" (Application (Application (Variable "z") (Variable "y")) (Variable "y"))
        it "λx.(z w) y" do
          shouldSame "x" (Application (Application (Variable "z") (Variable "w")) (Variable "y"))
    describe "λx.M" do
      describe "M = <variable>" do
        it "λx.x ≡ λx.x" do
          shouldSame "x" (Variable "x")
        it "λx.y ≡ λx.y" do
          shouldSame "x" (Variable "y")
      describe "M = λ<variable>.<variable>" do
        it "λx x.x ≡ λx x.x" do
          shouldSame "x" (LambdaAbstraction "x" (Variable "x"))
        it "λx x.y ≡ λx x.y" do
          shouldSame "x" (LambdaAbstraction "x" (Variable "y"))
        it "λx y.x ≡ λx y.x" do
          shouldSame "x" (LambdaAbstraction "y" (Variable "x"))
        it "λx y.z ≡ λx y.z" do
          shouldSame "x" (LambdaAbstraction "y" (Variable "z"))
  where
  shouldSame bound body = etaConversion bound body `shouldEqual` LambdaAbstraction bound body

testCallByValue :: Spec Unit
testCallByValue =
  describe "callByValue" do
    it "succ zero ≡ one" do
      eval (Application (Variable "succ") (Variable "zero")) `shouldEqual` vone
    it "succ one ≡ two" do
      eval (Application (Variable "succ") (Variable "one")) `shouldEqual` vtwo
    it "plus zero zero ≡ zero" do
      eval (Application (Application (Variable "plus") (Variable "zero")) (Variable "zero")) `shouldEqual` vzero
    it "plus zero one ≡ one" do
      eval (Application (Application (Variable "plus") (Variable "zero")) (Variable "one")) `shouldEqual` vone
    it "plus one zero ≡ one" do
      eval (Application (Application (Variable "plus") (Variable "one")) (Variable "zero")) `shouldEqual` vone
  where
  vzero = VLambdaAbstraction "f" (VLambdaAbstraction "x" (VVariable "x"))

  vone = VLambdaAbstraction "f" (VLambdaAbstraction "x" (VApplication $ VIdentApp "f" (VVariable "x")))

  vtwo = VLambdaAbstraction "f" (VLambdaAbstraction "x" (VApplication $ VIdentApp "f" (VApplication $ VIdentApp "f" (VVariable "x"))))

  eval expr = callByValue $ withEnvironment standardLibs expr
