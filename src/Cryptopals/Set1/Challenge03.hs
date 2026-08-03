-- Challenge 3: Single-byte XOR cipher
-- https://cryptopals.com/sets/1/challenges/3

module Cryptopals.Set1.Challenge03 (solve) where

import qualified Data.ByteString as B
import Data.Word (Word8)

import Cryptopals.Util (ByteBuffer, hexToBuffer, xorWithKey, englishScore, tryXorDecrypt)

solve :: String -> ByteBuffer
solve = tryXorDecrypt . hexToBuffer