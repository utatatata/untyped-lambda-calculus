module UntypedLambda.Repl
  ( eval
  , EvalError(..)
  ) where

import Prelude
import Data.Array (concatMap, index, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, take)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Pos (Position(..))
import UntypedLambda.Core (Identifier, Expression, Value, callByValue, Environment, withEnvironment)
import UntypedLambda.Parser (expression, identifier, spaces)

define :: Parser String (Tuple Identifier Expression)
define = do
  name <- identifier
  _ <- spaces *> string "=" <* spaces
  expr <- expression
  pure $ Tuple name expr

data EvalError
  = ParseError String

instance showEvalError :: Show EvalError where
  show (ParseError msg) = "(ParseError " <> msg <> ")"

instance eqEvalError :: Eq EvalError where
  eq (ParseError x) (ParseError y) = eq x y

eval :: Environment -> String -> Either EvalError (Tuple Value Environment)
eval env input = case runParser input define of
  Right (Tuple name expr) ->
    let
      value = callByValue (withEnvironment env expr)
    in
      pure $ Tuple value $ (Tuple name value) : env
  Left _ -> case runParser input expression of
    Right expr -> pure $ Tuple (callByValue $ withEnvironment env expr) env
    Left error ->
      let
        message = parseErrorMessage error

        inputLines =
          input
            # split (Pattern "\n")
            # concatMap (split (Pattern "\r"))

        Position { column, line } = parseErrorPosition error
      in
        case inputLines `index` (line - 1) of
          Just inputLine -> Left $ ParseError $ message <> " near '" <> take column inputLine <> "'"
          Nothing -> Left $ ParseError $ message
