-- Challenge 3: Single-byte XOR cipher
-- https://cryptopals.com/sets/1/challenges/3

module Cryptopals.Set1.Challenge03 (solve) where

import qualified Data.ByteString as B
import Data.Word (Word8)

import Cryptopals.Util (Bytes, hexToBytes, xorWithKey, englishScore, crackXorDecrypt, bytesToStr)

solve :: String -> String
solve = bytesToStr . fst . crackXorDecrypt . hexToBytes