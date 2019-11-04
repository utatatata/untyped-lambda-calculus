module UntypedLambda.Core.Term
  ( Identifier
  , Term(..)
  , NormalForm(..)
  , NFApplication(..)
  , asTerm
  ) where

import Prelude
import Data.Display (class Display, display)
import Data.Function (on)
import Data.String (drop, joinWith)

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
