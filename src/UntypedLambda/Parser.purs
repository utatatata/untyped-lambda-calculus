module UntypedLambda.Parser
  ( expression
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.Char.Unicode (isAlphaNum)
import Data.Foldable (foldl, foldr)
import Data.List (List(..), (:))
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepEndBy, between)
import Text.Parsing.Parser.String (eof, string, satisfy)
import Text.Parsing.Parser.Token (space)
import UntypedLambda.Core (Expression(..))

expression :: Parser String Expression
expression = spaces *> _expression <* eof
  where
  spaces = A.many $ void space

  spaces1 = A.some $ void space

  parens = between (string "(" <* spaces) (spaces *> string ")")

  identifier =
    fromCharArray
      <$> ( A.some
            $ satisfy \chr ->
                (isAlphaNum chr && chr /= 'λ') || chr `A.elem` [ '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '<', '>', '/', '?' ]
        )

  -- <variable> ::= <identifier>
  variable = Variable <$> identifier

  -- <lambda_abstraction> ::= λ <args> . <expression>
  -- <args> ::= <identifier> | <identifier> <spaces> <args>
  lambdaAbstraction expr = do
    _ <- (string "λ" <|> string "\\") <* spaces
    x <- identifier
    xs <- (spaces1 *> identifier `sepEndBy` spaces1) <|> pure Nil
    _ <- spaces *> string "." <* spaces
    body <- expr
    pure $ foldr LambdaAbstraction body (x : xs)

  -- <term> ::= ( <expression> ) | <variable> | <lambda_abstraction>
  term expr = parens expr <|> variable <|> lambdaAbstraction expr

  -- <expression> ::= <term> | <term> <spaces> <term> // <application> as an operator instead of a nonterminal symbol
  _expression =
    fix \expr -> do
      x <- term expr
      xs <- (spaces *> term expr `sepEndBy` spaces1) <|> pure Nil
      pure $ if xs == Nil then x else foldl Application x xs
