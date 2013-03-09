{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Eyeshadow.Data.Name
  (Name(..),
   NameComponent(..))
  where

import qualified Data.Text as T

import Eyeshadow.Prelude
   

data Name = Name [NameComponent]


data NameComponent = NameComponent T.Text
  deriving (Eq, Ord)
