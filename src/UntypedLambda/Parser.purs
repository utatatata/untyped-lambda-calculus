module UntypedLambda.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as A
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

  lambda = string "λ" <|> string "\\"

  dot = string "."

  identifier = fromCharArray <$> (A.some $ alphaNum <|> oneOf [ '!', '@', '#', '$', '%', '^', '&', '*', '-', '_', '+', '|', '<', '>', '/', '?' ])

  args = ((lambda <* spaces) `between` dot) $ sepEndBy identifier spaces1
  
  variable = Variable <$> identifier
  
  lambdaAbstraction e = (\ids expr -> foldr (\id body -> LambdaAbstraction id body) expr ids) <$> args <* spaces *> e

  application e = (\m n ls -> foldl Application (Application m n) ls) <$> e <*> (spaces1 *> e) <*> sepEndBy e spaces
  
  _expression = fix \e -> variable <|> lambdaAbstraction e <|> application e