-- Challenge 6: Break repeating-key XOR
-- https://cryptopals.com/sets/1/challenges/6

module Cryptopals.Set1.Challenge06 (solve, editDistance) where

import Cryptopals.Util (Bytes, xorBytes)
import Data.Bits (popCount)

solve :: String -> IO ()
solve filepath = return ()

editDistance :: Bytes -> Bytes -> Int
editDistance xs ys = sum $ (map popCount) $ xorBytes xs ys