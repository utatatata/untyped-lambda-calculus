module UntypedLambda.Repl
  ( repl
  ) where

import Prelude
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (string)
import UntypedLambda.Core (Identifier, Expression, Value, asExpression, callByValue, Environment, withEnvironment)
import UntypedLambda.Parser (expression, identifier, spaces)

define :: Parser String (Tuple Identifier Expression)
define = do
  name <- identifier
  _ <- spaces *> string "=" <* spaces
  expr <- expression
  pure $ Tuple name expr

repl :: Environment -> String -> Either ParseError (Tuple Value Environment)
repl env str = case runParser str define of
  Right (Tuple name expr) -> pure let value = callByValue (withEnvironment env expr) in Tuple value $ cons (Tuple name $ asExpression value) env
  Left _ -> runParser str expression <#> callByValue <<< (withEnvironment env) <#> Tuple `flip` env
