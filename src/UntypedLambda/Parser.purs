module UntypedLambda.Parser
  ( expression
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Foldable (foldl, foldr)
import Data.List (List(..), (:))
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepEndBy, between)
import Text.Parsing.Parser.String (eof, oneOf, string)
import Text.Parsing.Parser.Token (alphaNum, space)
import UntypedLambda.Core (Expression(..))

expression :: Parser String Expression
expression = spaces *> _expression <* eof
  where
  spaces = A.many $ void space

  spaces1 = A.some $ void space

  parens = between (string "(" <* spaces) (string ")")

  identifier = fromCharArray <$> (A.some $ alphaNum <|> oneOf [ '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '<', '>', '/', '?' ])

  -- <variable> ::= <identifier>
  variable = Variable <$> identifier

  -- <lambda abstraction> ::= λ<args>. <expression>
  -- <args> =:: <identifier> | <args> <identifier>
  lambdaAbstraction expr = do
    _ <- (string "λ" <|> string "\\") <* spaces
    x <- identifier
    xs <- (spaces1 *> sepEndBy identifier spaces1) <|> pure Nil
    _ <- spaces *> string "." <* spaces
    body <- expr
    pure $ foldr LambdaAbstraction body (x : xs)

  -- <application> =:: <expression> <expression> | <application> <expression>
  application expr = do
    m <- expr
    _ <- spaces1
    n <- expr
    ls <- (spaces1 *> sepEndBy expr spaces1) <|> pure Nil
    pure $ foldl Application (Application m n) ls

  -- <factor> ::= ( <lambda abstraction> ) | ( <application> )
  factor expr = do
    _ <- string "(" <* spaces
    _factor <- lambdaAbstraction expr <|> application expr
    _ <- spaces *> string ")"
    pure _factor

  -- <expression> ::= <variable> | <factor>
  _expression = fix \expr -> variable <|> factor expr
