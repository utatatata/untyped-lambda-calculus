module Main where

import Prelude
import Data.Function (on)
import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:), filter, union, notElem)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

onM :: forall f a b c. Apply f => (b -> b -> c) -> (a -> f b) -> a -> a -> f c
onM f g x y = f <$> g x <*> g y

data Expression
  = Identifier Identifier
  | LambdaAbstraction LambdaAbstraction
  | Application Application

instance eqExpression :: Eq Expression where
  eq (Identifier x) (Identifier y) = eq x y
  eq (LambdaAbstraction x) (LambdaAbstraction y) = eq x y
  eq (Application x) (Application y) = eq x y
  eq _ _ = false

instance showExpression :: Show Expression where
  show (Identifier x) = "(Identifier " <> show x <> ")"
  show (LambdaAbstraction x) = "(LambdaAbstraction " <> show x <> ")"
  show (Application x) = "(Application " <> show x <> ")"

type Identifier
  = String

type LambdaAbstraction
  = Tuple Identifier Expression

type Application
  = Tuple Expression Expression

free :: Expression -> List Identifier
free (Identifier id) = id : Nil

free (LambdaAbstraction (Tuple bound expr)) = filter (_ /= bound) $ free expr

free (Application (Tuple expr arg)) = (union `on` free) expr arg

shadow :: Identifier -> Expression -> Expression
shadow var expr = replace 1
  where
  freeVars = free expr

  replace :: Int -> Expression
  replace n =
    let
      shadowVar = var <> "_" <> (toStringAs decimal n)
    in
      if shadowVar `notElem` freeVars then
        alphaConversion (Substitution var (Identifier shadowVar)) expr
      else
        replace $ n + 1

data Substitution
  = Substitution Identifier Expression

alphaConversion :: Substitution -> Expression -> Expression
alphaConversion (Substitution match replacement) (Identifier id) = if match == id then replacement else (Identifier id)

alphaConversion sub@(Substitution match replacement) lambda@(LambdaAbstraction (Tuple bound expr)) = if match == bound then
  lambda
else if bound `notElem` free replacement then
  LambdaAbstraction (Tuple bound $ alphaConversion sub expr)
else
  LambdaAbstraction (Tuple bound $ alphaConversion (Substitution match $ shadow bound replacement) expr)

alphaConversion sub (Application (Tuple expr arg)) = Application $ (Tuple `on` (alphaConversion sub)) expr arg

betaReduction :: Application -> Expression
betaReduction (Tuple (LambdaAbstraction (Tuple bound expr)) arg) = alphaConversion (Substitution bound arg) expr

betaReduction app = Application app

etaConversion :: LambdaAbstraction -> Expression
etaConversion lambda@(Tuple bound (Application (Tuple expr (Identifier id)))) = if bound == id && bound `notElem` free expr then expr else LambdaAbstraction lambda

etaConversion lambda = LambdaAbstraction lambda

data NormalForm
  = NFIdentifier Identifier
  | NFLambdaAbstraction Identifier NormalForm
  | NFApplication NFApplication

instance eqNormalForm :: Eq NormalForm where
  eq (NFIdentifier x) (NFIdentifier y) = eq x y
  eq (NFLambdaAbstraction x f) (NFLambdaAbstraction y g) = eq x y && eq f g
  eq (NFApplication x) (NFApplication y) = eq x y
  eq _ _ = false

instance showNormalForm :: Show NormalForm where
  show (NFIdentifier id) = "(NFIdentifier " <> show id <> ")"
  show (NFLambdaAbstraction x f) = "(NFLambdaAbstraction " <> show x <> " " <> show f <> ")"
  show (NFApplication x) = "(NFApplication " <> show x <> ")"

data NFApplication
  = NFIdentApp Identifier NormalForm
  | NFAppApp NFApplication NormalForm

instance eqNFApplication :: Eq NFApplication where
  eq (NFIdentApp x f) (NFIdentApp y g) = eq x y && eq f g
  eq (NFAppApp x f) (NFAppApp y g) = eq x y && eq f g
  eq _ _ = false

instance showNFApplication :: Show NFApplication where
  show (NFIdentApp x f) = "(NFIdentApp " <> show x <> " " <> show f <> ")"
  show (NFAppApp x f) = "(NFAppApp " <> show x <> " " <> show f <> ")"

asExpression :: NormalForm -> Expression
asExpression (NFIdentifier id) = Identifier id

asExpression (NFLambdaAbstraction bound nf) = LambdaAbstraction $ Tuple bound $ asExpression nf

asExpression (NFApplication (NFIdentApp id nf)) = Application $ Tuple (Identifier id) $ asExpression nf

asExpression (NFApplication (NFAppApp nfApp nf)) = Application $ Tuple (asExpression $ NFApplication nfApp) $ asExpression nf

callByName :: Expression -> NormalForm
callByName (Identifier id) = NFIdentifier id

callByName (LambdaAbstraction (Tuple bound expr)) = NFLambdaAbstraction bound $ callByName expr

callByName (Application (Tuple (Identifier id) arg)) = NFApplication $ NFIdentApp id $ callByName arg

callByName (Application app@(Tuple (LambdaAbstraction _) _)) = callByName $ betaReduction app

callByName (Application (Tuple expr arg)) = case callByName expr of
  NFIdentifier id -> NFApplication $ NFIdentApp id $ callByName arg
  NFApplication nfApp -> NFApplication $ NFAppApp nfApp $ callByName arg
  NFLambdaAbstraction bound nf -> callByName $ Application $ Tuple (LambdaAbstraction $ Tuple bound $ asExpression nf) arg

main :: Effect Unit
main = do
  log "🍝"
