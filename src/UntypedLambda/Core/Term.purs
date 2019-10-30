module UntypedLambda.Core.Term
  ( Identifier
  , Term(..)
  , freeVariables
  , Substitution(..)
  , alphaConversion
  , betaReduction
  , etaConversion
  , NormalForm(..)
  , NFApplication(..)
  , asTerm
  , callByValue
  , Environment
  , withEnvironment
  ) where

import Prelude
import Control.Monad.Trampoline (Trampoline)
import Data.Array (filter, union, notElem)
import Data.Display (class Display, display)
import Data.Foldable (foldl, foldr)
import Data.Function (on)
import Data.Int (decimal, toStringAs)
import Data.String (drop, joinWith)
import Data.Tuple (Tuple(..))

type Identifier
  = String

data Term
  = Variable Identifier
  | LambdaAbstraction Identifier Term
  | Application Term Term

instance eqTerm :: Eq Term where
  eq (Variable x) (Variable y) = eq x y
  eq (LambdaAbstraction x m) (LambdaAbstraction y n) = eq x y && eq m n
  eq (Application x a) (Application y b) = eq x y && eq a b
  eq _ _ = false

instance showTerm :: Show Term where
  show (Variable x) = "(Variable " <> show x <> ")"
  show (LambdaAbstraction x m) = "(LambdaAbstraction " <> show x <> " " <> show m <> ")"
  show (Application x a) = "(Application " <> show x <> " " <> show a <> ")"

instance displayTerm :: Display Term where
  display :: Term -> String
  display = case _ of
    Variable x -> x
    -- drop 'λ'
    LambdaAbstraction bound body@(LambdaAbstraction _ _) -> "λ" <> bound <> " " <> (drop 1 $ display body)
    LambdaAbstraction bound body -> "λ" <> bound <> "." <> (display body)
    Application term arg ->
      joinWith " "
        [ case term of
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

freeVariables :: Term -> Array Identifier
freeVariables (Variable x) = [ x ]

freeVariables (LambdaAbstraction bound body) = freeVariables body # filter ((/=) bound)

freeVariables (Application term arg) = (union `on` freeVariables) term arg

data Substitution
  = Substitution Identifier Term

alphaConversion :: Substitution -> Term -> Term
alphaConversion (Substitution match replacement) (Variable x)
  | match == x = replacement

alphaConversion _ var@(Variable _) = var

alphaConversion (Substitution match _) lambda@(LambdaAbstraction bound _)
  | match == bound = lambda

alphaConversion sub@(Substitution _ replacement) (LambdaAbstraction bound body)
  | bound `notElem` freeVariables replacement = LambdaAbstraction bound $ alphaConversion sub body

alphaConversion sub@(Substitution _ replacement) lambda@(LambdaAbstraction bound body) = alphaConversion sub newLambda
  where
  newLambda = LambdaAbstraction newBound newBody

  newBody = alphaConversion (Substitution bound $ Variable newBound) body

  newBound = rename bound replacement

  -- rename x N, if x ∉ FV(N) then x else if x_0 ∉ FV(N) then x_0 else if x_1 ∉ FV(N) then x_1 else if ...
  rename :: String -> Term -> String
  rename var term =
    if var `notElem` freeVars then
      var
    else
      recur 0
    where
    freeVars = freeVariables term

    recur n =
      let
        renamed = var <> "_" <> (toStringAs decimal n)
      in
        if renamed `notElem` freeVars then renamed else recur $ n + 1

alphaConversion sub (Application term arg) = (Application `on` alphaConversion sub) term arg

betaReduction :: Term -> Term -> Term
betaReduction (LambdaAbstraction bound body) arg = alphaConversion (Substitution bound arg) body

betaReduction term arg = Application term arg

etaConversion :: Identifier -> Term -> Term
etaConversion bound body@(Application term (Variable var)) =
  if bound == var && bound `notElem` freeVariables term then
    term
  else
    LambdaAbstraction bound body

etaConversion bound body = LambdaAbstraction bound body

data NormalForm
  = NFVariable Identifier
  | NFLambdaAbstraction Identifier NormalForm
  | NFApplication NFApplication

data NFApplication
  = NFIdentApp Identifier NormalForm
  | NFAppApp NFApplication NormalForm

instance eqNormalForm :: Eq NormalForm where
  eq (NFVariable x) (NFVariable y) = eq x y
  eq (NFLambdaAbstraction x f) (NFLambdaAbstraction y g) = eq x y && eq f g
  eq (NFApplication x) (NFApplication y) = eq x y
  eq _ _ = false

instance showNormalForm :: Show NormalForm where
  show (NFVariable id) = "(NFVariable " <> show id <> ")"
  show (NFLambdaAbstraction x f) = "(NFLambdaAbstraction " <> show x <> " " <> show f <> ")"
  show (NFApplication x) = "(NFApplication " <> show x <> ")"

instance displayNormalForm :: Display NormalForm where
  display nomalForm = display $ asTerm nomalForm

instance eqNFApplication :: Eq NFApplication where
  eq (NFIdentApp v x) (NFIdentApp w y) = eq v w && eq x y
  eq (NFAppApp f x) (NFAppApp g y) = eq f g && eq x y
  eq _ _ = false

instance showNFApplication :: Show NFApplication where
  show (NFIdentApp x y) = "(NFIdentApp " <> show x <> " " <> show y <> ")"
  show (NFAppApp x y) = "(NFAppApp " <> show x <> " " <> show y <> ")"

asTerm :: NormalForm -> Term
asTerm (NFVariable id) = Variable id

asTerm (NFLambdaAbstraction bound body) = LambdaAbstraction bound $ asTerm body

asTerm (NFApplication (NFIdentApp id arg)) = Application (Variable id) $ asTerm arg

asTerm (NFApplication (NFAppApp nfapp arg)) = (Application `on` asTerm) (NFApplication nfapp) arg

callByValue :: Term -> Trampoline NormalForm
callByValue (Variable id) = pure $ NFVariable id

callByValue (LambdaAbstraction bound body) = NFLambdaAbstraction bound <$> callByValue body

callByValue (Application term arg) = do
  nfterm <- callByValue term
  nfarg <- callByValue arg
  case nfterm of
    NFVariable id -> pure $ NFApplication $ NFIdentApp id nfarg
    NFLambdaAbstraction _ _ -> callByValue $ (betaReduction `on` asTerm) nfterm nfarg
    NFApplication nfapp -> pure $ NFApplication $ NFAppApp nfapp nfarg

type Environment
  = Array (Tuple Identifier NormalForm)

withEnvironment :: Environment -> Term -> Term
withEnvironment env term =
  let
    names = env # map \(Tuple name _) -> name

    nomalForms = env # map \(Tuple _ nomalForm) -> asTerm nomalForm
  in
    foldl (\tm nomalForm -> Application tm nomalForm) (foldr (\name tm -> LambdaAbstraction name tm) term names) nomalForms
