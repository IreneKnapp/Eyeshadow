{-# LANGUAGE FlexibleInstances #-}
module Knapp.Show (Show(..)) where

import Prelude hiding (Show(..))

import Data.Text (Text)
import qualified Data.Text as T
import Numeric


class Show a where
  show :: a -> Text


instance Show Int where
  show value = T.pack $ showInt value ""

