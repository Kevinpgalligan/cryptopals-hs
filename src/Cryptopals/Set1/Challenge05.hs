-- Challenge 5: Implement repeating-key XOR
-- https://cryptopals.com/sets/1/challenges/5

-- The plaintext:
-- "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

module Cryptopals.Set1.Challenge05 (solve) where

import Cryptopals.Util (strToBytes, bytesToHex, xorBytes, encryptRepeatedKeyXor)

solve :: String -> String -> String
solve key text = bytesToHex
    $ encryptRepeatedKeyXor (strToBytes text) (strToBytes key)