module UntypedLambda.Core
  ( Identifier
  , Expression(..)
  , display
  , freeVariables
  , Substitution(..)
  , alphaConversion
  , betaReduction
  , etaConversion
  , Value(..)
  , VApplication(..)
  , asExpression
  , callByValue
  , Environment
  , withEnvironment
  , standardLibs
  ) where

import Prelude
import Data.Array (filter, union, notElem)
import Data.Foldable (foldl, foldr)
import Data.Function (on)
import Data.Int (decimal, toStringAs)
import Data.String (drop, joinWith)
import Data.Tuple (Tuple(..))

type Identifier
  = String

data Expression
  = Variable Identifier
  | LambdaAbstraction Identifier Expression
  | Application Expression Expression

instance eqExpression :: Eq Expression where
  eq (Variable x) (Variable y) = eq x y
  eq (LambdaAbstraction x m) (LambdaAbstraction y n) = eq x y && eq m n
  eq (Application x a) (Application y b) = eq x y && eq a b
  eq _ _ = false

instance showExpression :: Show Expression where
  show (Variable x) = "(Variable " <> show x <> ")"
  show (LambdaAbstraction x m) = "(LambdaAbstraction " <> show x <> " " <> show m <> ")"
  show (Application x a) = "(Application " <> show x <> " " <> show a <> ")"

display :: Expression -> String
display = case _ of
  Variable x -> x
  -- drop 'λ'
  LambdaAbstraction bound body@(LambdaAbstraction _ _) -> "λ" <> bound <> " " <> (drop 1 $ display body)
  LambdaAbstraction bound body -> "λ" <> bound <> "." <> (display body)
  Application expr arg ->
    joinWith " "
      [ case expr of
          Variable var -> var
          -- left-associative
          lambda@(LambdaAbstraction _ _) -> paren $ display lambda
          -- right-associative
          app@(Application _ _) -> display app
      , case arg of
          Variable var -> var
          -- left-associative, but it is unable to determin whether "m" in "l m n" is enclosed in parentheses or not in a LL(1) parser.
          -- It requires a LL(2) parser so "m" in "l m n" is always enclosed in parentheses.
          lambda@(LambdaAbstraction _ _) -> paren $ display lambda
          -- right-associative
          app@(Application _ _) -> paren $ display app
      ]
  where
  paren x = joinWith x [ "(", ")" ]

freeVariables :: Expression -> Array Identifier
freeVariables (Variable x) = [ x ]

freeVariables (LambdaAbstraction bound body) = freeVariables body # filter ((/=) bound)

freeVariables (Application expr arg) = (union `on` freeVariables) expr arg

data Substitution
  = Substitution Identifier Expression

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

type Environment
  = Array (Tuple Identifier Value)

withEnvironment :: Environment -> Expression -> Expression
withEnvironment env expression =
  let
    names = env # map \(Tuple name _) -> name

    values = env # map \(Tuple _ value) -> asExpression value
  in
    foldl (\expr value -> Application expr value) (foldr (\name expr -> LambdaAbstraction name expr) expression names) values

standardLibs :: Environment
standardLibs =
  [ Tuple "zero" $ VLambdaAbstraction "f" $ VLambdaAbstraction "x" $ VVariable "x"
  , Tuple "one"
      $ VLambdaAbstraction "f"
      $ VLambdaAbstraction "x"
      $ VApplication
      $ VIdentApp "f"
      $ VVariable "x"
  , Tuple "two"
      $ VLambdaAbstraction "f"
      $ VLambdaAbstraction "x"
      $ VApplication
      $ VIdentApp "f"
      $ VApplication
      $ VIdentApp "f"
      $ VVariable "x"
  , Tuple "three"
      $ VLambdaAbstraction "f"
      $ VLambdaAbstraction "x"
      $ VApplication
      $ VIdentApp "f"
      $ VApplication
      $ VIdentApp "f"
      $ VApplication
      $ VIdentApp "f"
      $ VVariable "x"
  , Tuple "succ"
      $ VLambdaAbstraction "n"
      $ VLambdaAbstraction "f"
      $ VLambdaAbstraction "x"
      $ VApplication
      $ VIdentApp "f"
      $ VApplication
      $ VAppApp (VIdentApp "n" $ VVariable "f")
      $ VVariable "x"
  , Tuple "plus"
      $ VLambdaAbstraction "m"
      $ VLambdaAbstraction "n"
      $ VLambdaAbstraction "f"
      $ VLambdaAbstraction "x"
      $ VApplication
      $ VAppApp (VIdentApp "m" $ VVariable "f")
      $ VApplication
      $ VAppApp (VIdentApp "n" $ VVariable "f")
      $ VVariable "x"
  , Tuple "mul"
      $ VLambdaAbstraction "m"
      $ VLambdaAbstraction "n"
      $ VLambdaAbstraction "f"
      $ VApplication
      $ VIdentApp "m"
      $ VApplication
      $ VIdentApp "f"
      $ VVariable "n"
  , Tuple "pow"
      $ VLambdaAbstraction "b"
      $ VLambdaAbstraction "e"
      $ VApplication
      $ VIdentApp "e"
      $ VVariable "b"
  , Tuple "pred"
      $ VLambdaAbstraction "n"
      $ VLambdaAbstraction "f"
      $ VLambdaAbstraction "x"
      $ VApplication
      $ VAppApp
          ( VAppApp
              ( VIdentApp "n"
                  $ VLambdaAbstraction "g"
                  $ VLambdaAbstraction "h"
                  $ VApplication
                  $ VIdentApp "h"
                  $ VApplication
                  $ VIdentApp "g"
                  $ VVariable "f"
              )
              $ VLambdaAbstraction "u"
              $ VVariable "x"
          )
      $ VLambdaAbstraction "u"
      $ VVariable "u"
  , Tuple "sub"
      $ VLambdaAbstraction "m"
      $ VLambdaAbstraction "n"
      $ VApplication
      $ VAppApp
          ( VIdentApp "n"
              $ VLambdaAbstraction "l"
              $ VLambdaAbstraction "f"
              $ VLambdaAbstraction "x"
              $ VApplication
              $ VAppApp
                  ( VAppApp
                      ( VIdentApp "l"
                          $ VLambdaAbstraction "g"
                          $ VLambdaAbstraction "h"
                          $ VApplication
                          $ VIdentApp "h"
                          $ VApplication
                          $ VIdentApp "g"
                          $ VVariable "f"
                      )
                      $ VLambdaAbstraction "u"
                      $ VVariable "x"
                  )
              $ VLambdaAbstraction "u"
              $ VVariable "u"
          )
      $ VVariable "m"
  ]