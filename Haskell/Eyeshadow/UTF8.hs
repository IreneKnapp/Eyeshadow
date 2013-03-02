module Foreign.C.UTF8 where

import Data.Bits
import Data.Char

expandCharacter :: Char -> String
expandCharacter c = do
	map chr $ case ord c of
		n
			| n <= 0x7F -> [n]
			| n <= 0x7FF -> [bitRange n 6 5 0xC0, bitRange n 0 6 0x80]
			| n <= 0xFFFF -> [bitRange n 12 4 0xE0, bitRange n 6 6 0x80, bitRange n 0 6 0x80]
			| n <= 0x10FFFF -> [bitRange n 18 3 0xF0, bitRange n 12 6 0x80, bitRange n 6 6 0x80,
				bitRange n 0 6 0x80]
			| otherwise -> error $ "Value " ++ (show n) ++ " too big for UTF8."
	where bitRange n start length highBits =
		((n `shiftR` start) `mod` (1 `shiftL` length)) .|. highBits

expandString :: String -> String
expandString string = foldl (++) "" $ map expandCharacter string

characterLengthFromLeadByte :: Char -> Int
characterLengthFromLeadByte c =
	case ord c of
		n
			| n >= 0 && n <= 0x7F -> 1
			| n >= 0xC2 && n <= 0xDF -> 2
			| n >= 0xE0 && n <= 0xEF -> 3
			| n >= 0xF0 && n <= 0xF4 -> 4
			| otherwise -> error $ "Invalid UTF8."

collapseCharacter :: String -> Char
collapseCharacter string@(lead : _) =
	chr $ case characterLengthFromLeadByte lead of
		1 -> ord $ string !! 0
		2 -> foldl (.|.) 0 $ zipWith bitRange string [(6, 0x1F), (0, 0x3F)]
		3 -> foldl (.|.) 0 $ zipWith bitRange string [(12, 0x0F), (6, 0x3F), (0, 0x3F)]
		4 -> foldl (.|.) 0 $ zipWith bitRange string [(18, 0x07), (12, 0x3F), (6, 0x3F), (0, 0x3F)]
	where bitRange c (start, lowBits) = ((ord c) .&. lowBits) `shiftL` start

collapseString :: String -> String
collapseString "" = ""
collapseString string@(lead : _) =
	let (left, right) = splitAt (characterLengthFromLeadByte lead) string
	in collapseCharacter left : collapseString right
