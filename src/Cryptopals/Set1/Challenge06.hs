-- Challenge 6: Break repeating-key XOR
-- https://cryptopals.com/sets/1/challenges/6

module Cryptopals.Set1.Challenge06 (solve, editDistance) where

import Data.Bits (popCount)
import Data.List (sortBy)
import Data.Ord (comparing)

import Cryptopals.Util (Bytes, xorBytes)

solve :: String -> IO ()
solve filepath = return ()

-- ghci> (strToBytes "this is a test") `editDistance` (strToBytes "wokka wokka!!!")
-- 37
editDistance :: Bytes -> Bytes -> Int
editDistance xs ys = sum $ (map popCount) $ xorBytes xs ys

rankKeySizes :: Bytes -> Int -> [Int]
rankKeySizes bs maxKeySize =
    map fst
    $ sortBy (comparing snd)
    $ map (\k -> (k, scoreKeySize bs k)) [2..maxKeySize]

scoreKeySize :: Bytes -> Int -> Double
scoreKeySize bs k = (fromIntegral $ editDistance (take k bs) (take k (drop k bs)))
  / (fromIntegral k)