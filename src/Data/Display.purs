module Data.Display
  ( class Display
  , display
  )
  where

class Display a where
  display :: a -> String