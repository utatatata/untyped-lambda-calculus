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
      if var `notElem` freeVars then
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

data NFApplication
  = NFIdentApp Identifier NormalForm
  | NFAppApp NFApplication NormalForm

main :: Effect Unit
main = do
  log "🍝"
