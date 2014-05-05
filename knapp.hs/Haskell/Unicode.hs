{-# LANGUAGE OverloadedStrings #-}
module Unicode (categorize) where

import Prelude hiding (Show(..))

import Data.Char
import qualified Data.Text as T
import Numeric

import Knapp.Show
import Unicode.Categories (categorize)


instance Show Char where
  show '\\' = "'\\\\'"
  show '\'' = "'\\\''"
  show c =
    let maybeCategory = categorize c
        digits = T.pack $ map toUpper (showHex (ord c) "")
        padding = T.pack $ take (4 - T.length digits) (repeat '0')
        asHex = T.concat ["U+", padding, digits]
        asText = T.concat ["'", T.singleton c, "'"]
    in case maybeCategory of
         Nothing -> asHex
         Just ['C', _] -> asHex
         _ -> asText

