module UntypedLambda.Parser
  ( expression
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
import Data.List (List(..))
import Data.Foldable (foldl, foldr)
import Data.String.CodeUnits (fromCharArray)
import UntypedLambda.Core (Expression(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepEndBy, between)
import Text.Parsing.Parser.String (eof, oneOf, string)
import Text.Parsing.Parser.Token (alphaNum, space)

expression :: Parser String Expression
expression = spaces *> _expression <* eof
  where
  spaces = A.many $ void space

  spaces1 = A.some $ void space

  parens = between (string "(" <* spaces) (string ")")

  identifier = fromCharArray <$> (A.some $ alphaNum <|> oneOf [ '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '<', '>', '/', '?' ])

  variable = Variable <$> identifier

  -- args = ((lambda <* spaces) `between` dot) $ sepEndBy identifier spaces1
  -- lambdaAbstraction e = parens $ (\ids expr -> foldr (\id body -> LambdaAbstraction id body) expr ids) <$> args <* spaces *> e
  args = do
    _ <- string "λ" <|> string "\\"
    _ <- spaces
    ids <- sepEndBy identifier spaces1
    _ <- spaces
    _ <- string "."
    pure $ ids
  
  lambdaAbstraction expr = do
    ids <- args
    _ <- spaces
    body <- expr
    pure $ foldr LambdaAbstraction body ids

  -- application e = parens $ (\m n ls -> foldl Application (Application m n) ls) <$> e <*> (spaces1 *> e) <*> sepEndBy e spaces1
  -- application e = parens (sepEndBy e spaces1)
  --   <#> \exprs -> foldl Application (Variable "___________") exprs
  application expr = do
    _ <- string "("
    _ <- spaces
    m <- expr
    _ <- spaces1
    n <- expr
    ls <- (spaces1 *> sepEndBy expr spaces1) <|> pure Nil
    _ <- spaces
    _ <- string ")"
    pure $ foldl Application (Application m n) ls

  _expression = fix \expr -> variable <|> lambdaAbstraction expr <|> application expr
