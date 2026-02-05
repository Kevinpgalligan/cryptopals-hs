-- Challenge 1: Convert hex to base64
-- https://cryptopals.com/sets/1/challenges/1
--
-- This ain't gonna work for larger inputs, need to handle
-- the input string piece-by-piece.
-- To that end...
--   byte   = 2^8
--   hex    = 2^4 -> 2 hex = 1 byte
--   base64 = 2^6 -> 4 base64 = 3 bytes = 6 hex
--            (since lcm(6,8)=24=4 base64=3 bytes)
-- So, the idea would be to take 6 hex characters at a time and
-- convert them into 4 base64 characters. Need to pad with zeros
-- if the number of hex characters isn't divisible by 6. Hm, that's
-- a bit awkward because then we can't distinguish between zeros in
-- the data vs padding. A problem for another day.
-- As it happens, the sample input consists of 96 hex characters, which
-- is divisible by 6.

module Cryptopals.Set1.Challenge01 (hexToBase64) where

import Cryptopals.Util (hexToNum, numToBase64)

hexToBase64 :: String -> String
hexToBase64 = numToBase64 . hexToNum
