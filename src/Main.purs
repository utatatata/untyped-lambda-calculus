module Main where

import Prelude
import Data.Array (filter, union, notElem)
import Data.Function (on)
import Data.Int (decimal, toStringAs)
import Effect (Effect)
import Effect.Console (log)

type Identifier
  = String

data Expression
  = Variable Identifier
  | LambdaAbstraction Identifier Expression
  | Application Expression Expression

data Substitution
  = Substitution Identifier Expression

instance eqExpression :: Eq Expression where
  eq (Variable x) (Variable y) = eq x y
  eq (LambdaAbstraction x m) (LambdaAbstraction y n) = eq x y && eq m n
  eq (Application x a) (Application y b) = eq x y && eq a b
  eq _ _ = false

instance showExpression :: Show Expression where
  show (Variable x) = "(Variable " <> show x <> ")"
  show (LambdaAbstraction x m) = "(LambdaAbstraction " <> show x <> " " <> show m <> ")"
  show (Application x a) = "(Application " <> show x <> " " <> show a <> ")"

freeVariables :: Expression -> Array Identifier
freeVariables (Variable x) = [ x ]

freeVariables (LambdaAbstraction bound body) = freeVariables body # filter ((/=) bound)

freeVariables (Application expr arg) = (union `on` freeVariables) expr arg

alphaConversion :: Substitution -> Expression -> Expression
alphaConversion (Substitution match replacement) (Variable x)
  | match == x = replacement

alphaConversion _ var@(Variable _) = var

alphaConversion (Substitution match _) lambda@(LambdaAbstraction bound _)
  | match == bound = lambda

alphaConversion sub@(Substitution _ replacement) (LambdaAbstraction bound body)
  | bound `notElem` freeVariables replacement = LambdaAbstraction bound $ alphaConversion sub body

alphaConversion (Substitution match replacement) lambda@(LambdaAbstraction bound body) =
  let
    freeVars = freeVariables replacement

    shadowVar n = let var = bound <> "_" <> (toStringAs decimal n) in if var `notElem` freeVars then var else shadowVar (n + 1)

    newReplacement = alphaConversion (Substitution bound (Variable $ shadowVar 0)) replacement
  in
    alphaConversion (Substitution match newReplacement) lambda

alphaConversion sub (Application expr arg) = (Application `on` alphaConversion sub) expr arg

betaReduction :: Expression -> Expression -> Expression
betaReduction (LambdaAbstraction bound body) arg = alphaConversion (Substitution bound arg) body

betaReduction expr arg = Application expr arg

etaConversion :: Identifier -> Expression -> Expression
etaConversion bound body@(Application expr (Variable var)) = if bound == var && bound `notElem` freeVariables expr then
  expr
else
  LambdaAbstraction bound body

etaConversion bound body = LambdaAbstraction bound body

main :: Effect Unit
main = do
  log "🍝"
