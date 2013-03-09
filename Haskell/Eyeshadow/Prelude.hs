{-# LANGUAGE NoImplicitPrelude #-}
module Eyeshadow.Prelude
  (P.Bool(..), (P.&&), (P.||), P.not, P.otherwise,
   P.Eq(..),
   P.Int,
   P.Integer,
   P.Integral(..), P.fromIntegral,
   P.Num(..),
   P.Ord(..),
   Show(..),
   P.String,
   (P.$), (P..),
   (P.>>=), P.return)
  where

import qualified Data.Text as T
import qualified Prelude as P


class Show showable where
  show :: showable -> T.Text

