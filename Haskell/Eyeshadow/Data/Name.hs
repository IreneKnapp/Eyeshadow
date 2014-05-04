{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveGeneric #-}
module Eyeshadow.Data.Name
  (Name(..),
   NameComponent(..))
  where

import qualified Data.Text as Text
import qualified Data.Hashable as Hashable

import GHC.Generics (Generic)

import Eyeshadow.Prelude
   

data Name = Name [NameComponent]


data NameComponent = NameComponent Text.Text
  deriving (Eq, Ord, Generic)
instance Hashable.Hashable NameComponent
