module Main where

import Prelude
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:), filter, union, notElem)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

onM :: forall f a b c. Apply f => (b -> b -> c) -> (a -> f b) -> a -> a -> f c
onM f g x y = f <$> g x <*> g y

type Identifier
  = String
type LambdaAbstraction = Tuple Identifier Expression
type Application = Tuple Expression Expression

data Expression
  = Identifier Identifier
  | LambdaAbstraction LambdaAbstraction
  | Application Application

free :: Expression -> List Identifier
free (Identifier var) = var : Nil
free (LambdaAbstraction (Tuple bound expr)) = filter (_ /= bound) $ free expr
free (Application (Tuple expr arg)) = (union `on` free) expr arg

type Substitution = Tuple Identifier Expression

alphaConversion :: Substitution -> Expression -> Maybe Expression
alphaConversion (Tuple var redex) (Identifier id) = pure if var == id then redex else (Identifier id)
alphaConversion sub@(Tuple var redex) lambda@(LambdaAbstraction (Tuple bound expr)) = if var == bound then pure lambda else
    if notElem bound $ free redex then
      LambdaAbstraction <$> Tuple bound <$> (alphaConversion sub expr)
    else
      Nothing

alphaConversion sub (Application (Tuple expr arg)) = Application <$> (Tuple `onM` alphaConversion sub) expr arg

betaReduction :: Application -> Maybe Expression
betaReduction (Tuple (LambdaAbstraction (Tuple bound expr)) arg) = alphaConversion (Tuple bound arg) expr
betaReduction _ = Nothing

etaConversion :: Expression -> Maybe Expression
etaConversion (Application (Tuple (LambdaAbstraction (Tuple bound expr)) (Identifier arg))) = if bound == arg then
  pure expr
else
  Nothing

etaConversion _ = Nothing

main :: Effect Unit
main = do
  log "🍝"
