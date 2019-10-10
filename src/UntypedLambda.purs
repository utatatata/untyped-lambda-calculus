module UntypedLambda
  ( module UntypedLambda.Core
  , module UntypedLambda.Repl
  , display
  ) where

import Prelude
import UntypedLambda.Core (Value(..), VApplication(..), Environment, withEnvironment, standardLibs)
import UntypedLambda.Core as Core
import UntypedLambda.Repl (eval)

display :: Value -> String
display = Core.display <<< Core.asExpression
