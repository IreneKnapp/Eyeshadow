{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable #-}
module Eyeshadow.Data.UTF8
  (EncodingFailure(..),
   DecodingFailure(..),
   encode,
   decode)
  where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Prelude

import Data.Bits
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import Data.Typeable

import Eyeshadow.Prelude


data EncodingFailure
  = OutOfRangeEncodingFailure Int
  deriving (Prelude.Show, Typeable)
instance Exception.Exception EncodingFailure


data DecodingFailure
  = InsufficientDataDecodingFailure
  | InvalidDataDecodingFailure
  deriving (Prelude.Show, Typeable)
instance Exception.Exception DecodingFailure


encode :: Char -> Either EncodingFailure BS.ByteString
encode c =
  case ord c of
    n | n <= 0x7F -> Right $ BS.pack [bitRange n 0 7 0x00]
      | n <= 0x7FF -> Right $ BS.pack [bitRange n 6 5 0xC0,
                                       bitRange n 0 6 0x80]
      | n <= 0xFFFF -> Right $ BS.pack [bitRange n 12 4 0xE0,
                                        bitRange n 6 6 0x80,
                                        bitRange n 0 6 0x80]
      | n <= 0x10FFFF -> Right $ BS.pack [bitRange n 18 3 0xF0,
                                          bitRange n 12 6 0x80,
                                          bitRange n 6 6 0x80,
                                          bitRange n 0 6 0x80]
      | otherwise -> Left $ OutOfRangeEncodingFailure n
  where bitRange n start length highBits =
          (fromIntegral $ (n `shiftR` start) `mod` (1 `shiftL` length))
          .|. highBits


decode :: BS.ByteString -> Either DecodingFailure (Char, BS.ByteString)
decode byteString =
  if BS.null byteString
    then Left InsufficientDataDecodingFailure
    else let leadByte = BS.index byteString 0
             maybeNBytes = characterLengthFromLeadByte leadByte
         in case maybeNBytes of
              Nothing -> Left InvalidDataDecodingFailure
              Just nBytes ->
                if BS.length byteString < nBytes
                  then Left InsufficientDataDecodingFailure
                  else let (head, rest) = BS.splitAt nBytes byteString
                           bytes = BS.unpack head
                           valid = all validFollowByte $ tail bytes
                           result =
                             chr $ foldl (.|.) 0 $ zipWith bitRange bytes
                               $ case nBytes of
                                   1 -> [(0, 0x7F)]
                                   2 -> [(6, 0x1F), (0, 0x3F)]
                                   3 -> [(12, 0x0F), (6, 0x3F), (0, 0x3F)]
                                   4 -> [(18, 0x07), (12, 0x3F), (6, 0x3F),
                                         (0, 0x3F)]
                                   _ -> []
                       in if valid
                            then Right (result, rest)
                            else Left InvalidDataDecodingFailure
  where bitRange byte (start, lowBits) =
          (fromIntegral byte .&. lowBits) `shiftL` start
        characterLengthFromLeadByte byte =
          case byte of
            n | n >= 0 && n <= 0x7F -> Just 1
              | n >= 0xC2 && n <= 0xDF -> Just 2
              | n >= 0xE0 && n <= 0xEF -> Just 3
              | n >= 0xF0 && n <= 0xF4 -> Just 4
              | otherwise -> Nothing
        validFollowByte byte =
          byte .&. 0xC0 == 0x80
