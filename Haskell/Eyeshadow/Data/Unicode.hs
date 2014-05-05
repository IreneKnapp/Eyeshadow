{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Eyeshadow.Data.Unicode (categorize) where

import qualified Data.Text as Text

import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Numeric

import Eyeshadow.Data.Unicode.Categories
import Eyeshadow.Prelude


instance Show Char where
  show '\\' = "'\\\\'"
  show '\'' = "'\\\''"
  show c =
    let maybeCategory = categorize c
        digits = Text.pack $ map toUpper (showHex (ord c) "")
        padding = Text.pack $ take (4 - Text.length digits) (repeat '0')
        asHex = Text.concat ["U+", padding, digits]
        asText = Text.concat ["'", Text.singleton c, "'"]
    in case fmap Text.head maybeCategory of
         Nothing -> asHex
         Just 'C' -> asHex
         _ -> asText
