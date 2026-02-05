-- Challenge 3: Single-byte XOR cipher
-- https://cryptopals.com/sets/1/challenges/3

module Cryptopals.Set1.Challenge03 (solve, decrypt) where

import qualified Data.ByteString as B
import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.Word (Word8)

import Cryptopals.Util (ByteBuffer, hexToBuffer, xorWithKey, englishScore)

solve :: String -> ByteBuffer
solve = decrypt . hexToBuffer

decrypt :: ByteBuffer -> ByteBuffer
decrypt buff = xorWithKey buff bestKey
  where
    bestKey = minimumBy (comparing (englishScore buff)) [1..maxBound]