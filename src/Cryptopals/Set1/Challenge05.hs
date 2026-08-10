-- Challenge 5: Implement repeating-key XOR
-- https://cryptopals.com/sets/1/challenges/5

-- The plaintext:
-- "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

module Cryptopals.Set1.Challenge05 (solve) where

import Cryptopals.Util (strToBytes, bytesToHex, xorBytes)

-- Takes a key and a string, and outputs a hex-encoded encryption of that string (using repeating-key XOR, of course).
-- TODO:
--  1. Function to create Byte from string.
--  2. Unpack function to get a list of Word8.
--  3. [k+c | k <- key, c <- text]    (except the [Word8] versions)
--  4. Pack as a ByteBuffer again.
--  5. Convert to hex.
--  6. Ponder my life choices. In particular, whether it's worth using ByteBuffer when I have to unpack it all the time.
-- The test string: "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
solve :: String -> String -> String
solve key text = bytesToHex
    $ xorBytes (cycleList $ strToBytes key) (strToBytes text)

cycleList :: [a] -> [a]
cycleList [] = []
cycleList xs = loop xs xs
    where loop xs [] = loop xs xs
          loop xs (y:ys) = y:(loop xs ys)