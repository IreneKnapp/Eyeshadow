module Knapp.Show (Show(..)) where

import Prelude hiding (Show(..))

import Data.Text (Text)


class Show a where
  show :: a -> Text

