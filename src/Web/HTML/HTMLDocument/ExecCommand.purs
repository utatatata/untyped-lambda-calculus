module Web.HTML.HTMLDocument.ExecCommand
  ( execCommand
  , CommandName
  , copy
  ) where

import Effect (Effect)
import Web.HTML.HTMLDocument (HTMLDocument)

newtype CommandName
  = CommandName String

copy :: CommandName
copy = CommandName "copy"

foreign import execCommand :: CommandName -> HTMLDocument -> Effect Boolean
