module UntypedLambda.REPL
  ( module UntypedLambda.Core.Term
  , REPL(..)
  , InputMode(..)
  , init
  , eval
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Trampoline (runTrampoline)
import Data.Array (index, snoc)
import Data.Display (display)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), joinWith, split, take)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (parseErrorMessage, parseErrorPosition, runParser) as P
import Text.Parsing.Parser.Combinators (try) as P
import Text.Parsing.Parser.Pos (Position(..)) as P
import Text.Parsing.Parser.String (eof, string) as P
import UntypedLambda.Core.Term (Environment)
import UntypedLambda.Core.Term as T
import UntypedLambda.Core.Parser (term, identifier, spaces) as P
import UntypedLambda.Core.Prime as Prime

newtype REPL
  = REPL
  { env :: T.Environment
  , history :: Array { input :: String, output :: Maybe String }
  , inputMode :: InputMode
  , inputPool :: Array String
  }

derive instance newtypeREPL :: Newtype REPL _

derive instance eqREPL :: Eq REPL

data InputMode
  = Singleline
  | Multiline

instance eqInputMode :: Eq InputMode where
  eq Singleline Singleline = true
  eq Multiline Multiline = true
  eq _ _ = false

instance showInputMode :: Show InputMode where
  show Singleline = "Singleline"
  show Multiline = "Multiline"

data Result
  = Term T.Term
  | Definition String T.Term
  | StartMultiline
  | EndMultiline
  | Help

init :: REPL
init =
  REPL
    { env: _init.env
    , history: []
    , inputMode: Singleline
    , inputPool: []
    }
  where
  REPL _init =
    Prime.prime
      # split (Pattern "\n")
      # foldl (flip eval)
          ( REPL
              { env: []
              , history: []
              , inputMode: Singleline
              , inputPool: []
              }
          )

eval :: String -> REPL -> REPL
eval input (REPL repl@{ env: _, history: _, inputMode: Multiline, inputPool }) = case P.runParser input endMultiline of
  Right _ ->
    eval (joinWith "\n" repl.inputPool) $ REPL
      $ repl
          { inputMode = Singleline
          , inputPool = []
          }
  Left _ -> REPL $ repl { inputPool = inputPool `snoc` input }
  where
  endMultiline = EndMultiline <$ (P.spaces *> P.string ".end" <* P.spaces <* P.eof)

eval input (REPL repl) = case P.runParser input parser of
  Right StartMultiline ->
    REPL
      $ repl
          { history = repl.history `snoc` { input, output: Nothing }
          , inputMode = Multiline
          }
  Right EndMultiline ->
    REPL
      $ repl
          { history =
            repl.history
              `snoc`
                { input, output: Just "Error: `.end` can only be used in a multiline mode." }
          }
  Right Help ->
    REPL
      $ repl
          { history =
            repl.history
              `snoc`
                { input
                , output:
                  Just
                    """The following commands are available.

.multi                        Start a multiline mode
.end                          End a multiline mode
.help                         Show this help menu

<identifier> = <term>   Add a new binding into the Environment. An term `M` under the Environment `a = b` is equivalent to ((λa.M) b)"""
                }
          }
  Right (Definition name tm) ->
    let
      normalForm = runTrampoline $ T.callByValue $ T.withEnvironment repl.env tm
    in
      REPL
        $ repl
            { env = repl.env `snoc` Tuple name normalForm
            , history =
              repl.history
                `snoc`
                  { input, output: Just $ display $ T.asTerm normalForm }
            }
  Right (Term tm) ->
    REPL
      $ repl
          { history =
            repl.history
              `snoc`
                { input, output: Just $ display $ T.asTerm $ runTrampoline $ T.callByValue $ T.withEnvironment repl.env tm }
          }
  Left error ->
    let
      msg = P.parseErrorMessage error

      P.Position { line, column } = P.parseErrorPosition error

      inputLines = do
        line <- split (Pattern "\n") input
        split (Pattern "\r`") line

      nearStr =
        inputLines `index` (line - 1)
          # maybe "" (take column)
    in
      REPL
        $ repl
            { history =
              repl.history
                `snoc`
                  { input, output: Just $ "Error: " <> msg <> " near '" <> nearStr <> "'." }
            }
  where
  startMultiline = StartMultiline <$ (P.spaces *> P.string ".multi" <* P.spaces <* P.eof)

  endMultiline = EndMultiline <$ (P.spaces *> P.string ".end" <* P.spaces <* P.eof)

  help = Help <$ (P.spaces *> P.string ".help" <* P.spaces <* P.eof)

  define = do
    _ <- P.spaces
    name <- P.identifier
    _ <- P.spaces *> P.string "=" <* P.spaces
    body <- P.term
    pure $ Definition name body

  _term = Term <$> P.term

  parser = P.try startMultiline <|> P.try endMultiline <|> P.try help <|> P.try define <|> _term
