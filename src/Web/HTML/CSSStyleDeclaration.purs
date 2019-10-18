module Web.HTML.CSSStyleDeclaration
  ( CSSStyleDeclaration
  , getComputedStyle
  , lineHeight
  ) where

import Effect (Effect)
import Web.DOM (Element)
import Web.HTML.Window (Window)

foreign import data CSSStyleDeclaration :: Type

foreign import getComputedStyle :: Element -> Window -> Effect CSSStyleDeclaration

foreign import lineHeight :: CSSStyleDeclaration -> Effect Number
