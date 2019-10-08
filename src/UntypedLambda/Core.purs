module UntypedLambda.Core where

import Prelude
import Data.Array (filter, union, notElem)
import Data.Foldable (foldl, foldr)
import Data.Function (on)
import Data.Tuple (Tuple(..))
import Data.Int (decimal, toStringAs)

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

data Value
  = VVariable Identifier
  | VLambdaAbstraction Identifier Value
  | VApplication VApplication

data VApplication
  = VIdentApp Identifier Value
  | VAppApp VApplication Value

instance eqValue :: Eq Value where
  eq (VVariable x) (VVariable y) = eq x y
  eq (VLambdaAbstraction x f) (VLambdaAbstraction y g) = eq x y && eq f g
  eq (VApplication x) (VApplication y) = eq x y
  eq _ _ = false

instance eqVApplication :: Eq VApplication where
  eq (VIdentApp v x) (VIdentApp w y) = eq v w && eq x y
  eq (VAppApp f x) (VAppApp g y) = eq f g && eq x y
  eq _ _ = false

instance showValue :: Show Value where
  show (VVariable id) = "(VVariable " <> show id <> ")"
  show (VLambdaAbstraction x f) = "(VLambdaAbstraction " <> show x <> " " <> show f <> ")"
  show (VApplication x) = "(VApplication " <> show x <> ")"

instance showVApplication :: Show VApplication where
  show (VIdentApp x y) = "(VIdentApp " <> show x <> " " <> show y <> ")"
  show (VAppApp x y) = "(VAppApp " <> show x <> " " <> show y <> ")"

asExpression :: Value -> Expression
asExpression (VVariable id) = Variable id

asExpression (VLambdaAbstraction bound body) = LambdaAbstraction bound $ asExpression body

asExpression (VApplication (VIdentApp id arg)) = Application (Variable id) $ asExpression arg

asExpression (VApplication (VAppApp vApp arg)) = (Application `on` asExpression) (VApplication vApp) arg

callByValue :: Expression -> Value
callByValue (Variable id) = VVariable id

callByValue (LambdaAbstraction bound body) = VLambdaAbstraction bound $ callByValue body

callByValue (Application expr arg) =
  callByValue expr
    # case _ of
        VVariable id -> VApplication $ VIdentApp id $ callByValue arg
        vlambda@(VLambdaAbstraction _ _) -> callByValue $ (betaReduction `on` asExpression) vlambda $ callByValue arg
        VApplication vApp -> VApplication $ VAppApp vApp $ callByValue arg

withEnvironment :: Array (Tuple Identifier Expression) -> Expression -> Expression
withEnvironment libs expression =
  let
    names = libs # map \(Tuple name _) -> name

    values = libs # map \(Tuple _ value) -> value
  in
    foldl (\expr value -> Application expr value) (foldr (\name expr -> LambdaAbstraction name expr) expression names) values

standardLibs :: Array (Tuple Identifier Expression)
standardLibs =
  [ Tuple "zero" $ LambdaAbstraction "f" (LambdaAbstraction "x" (Variable "x"))
  , Tuple "one" $ LambdaAbstraction "f" (LambdaAbstraction "x" (Application (Variable "f") (Variable "x")))
  , Tuple "two" $ LambdaAbstraction "f" (LambdaAbstraction "x" (Application (Variable "f") (Application (Variable "f") (Variable "x"))))
  , Tuple "three" $ LambdaAbstraction "f" (LambdaAbstraction "x" (Application (Variable "f") (Application (Variable "f") (Application (Variable "f") (Variable "x")))))
  , Tuple "succ" $ LambdaAbstraction "n" (LambdaAbstraction "f" (LambdaAbstraction "x" (Application (Variable "f") (Application (Application (Variable "n") (Variable "f")) (Variable "x")))))
  , Tuple "plus" $ LambdaAbstraction "m" (LambdaAbstraction "n" (LambdaAbstraction "f" (LambdaAbstraction "x" (Application (Application (Variable "m") (Variable "f")) (Application (Application (Variable "n") (Variable "f")) (Variable "x"))))))
  ]
