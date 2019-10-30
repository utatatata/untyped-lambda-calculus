module UntypedLambda.Core.Evaluator where

import Prelude
import Data.Array (filter, notElem, union)
import Data.Foldable (foldl, foldr)
import Data.Function (on)
import Data.Int (decimal, toStringAs)
import Control.Monad.Trampoline (Trampoline)
import Data.Tuple (Tuple(..))
import UntypedLambda.Core.Term (Identifier, Term(..), NormalForm(..), NFApplication(..), asTerm)

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
