-- Challenge 6: Break repeating-key XOR
-- https://cryptopals.com/sets/1/challenges/6

module Cryptopals.Set1.Challenge06 (solve, rankKeySizes, crackRepeatedKeyXor) where

import Data.Bits (popCount)
import Data.List (sortBy, transpose, minimumBy)
import Data.Ord (comparing)
import System.IO (hClose, hGetContents, openFile, IOMode (ReadMode))
import Data.Word (Word8)

import Cryptopals.Util (Bytes, xorBytes, decodeBase64, bytesToStr,
    crackXorDecrypt, englishScore)

-- Debugging!
-- These are the suggested keys, for different key sizes.
-- Why are they all vowels!? Probably because they key size is
-- wrong and it just picks the most common letters in the key to
-- try to maximise the number of times that a key byte ends up matched
-- with the appropriate character... I think.
--    ["ooiio","ioi","ii","ieoonoieiioii"]

-- Apparently, the challenge description wasn't clear. When you're checking the
-- edit distance (trying to identify the key size), you take N blocks, and compare
-- them to ALL THE OTHER BLOCKS OF THAT SIZE. And average.
-- So, I think the key size score should be:
--    (1/KeySize) * \sum_{B1}(\sum_{B2} EditDistance(B1, B2))/NumBlocks,
-- where NumBlocks = MessageLength/KeySize,
--       B1 are the blocks you take from the start, and
--       B2 are all the blocks.

-- My version does not identify the correct key size.

solve :: String -> IO ()
solve filepath = do
    handle <- openFile filepath ReadMode
    content <- hGetContents handle
    let bs = decodeBase64 $ foldr (++) [] (lines content)
    -- let (msg, key) = crackRepeatedKeyXor bs 40
    print $ map bytesToStr $ (crackRepeatedKeyXor bs 40)
    -- print (bytesToStr key)
    -- print "======"
    -- print (bytesToStr msg)
    hClose handle

-- Returns the decrypted message (hopefully) and associated key.
--crackRepeatedKeyXor :: Bytes -> Int -> (Bytes, Bytes)
crackRepeatedKeyXor :: Bytes -> Int -> [Bytes]
crackRepeatedKeyXor bs maxKeySize =
        -- (\(msg, key, _) -> (msg, key))
        -- $ minimumBy (comparing (\(_, _, score) -> score))
        -- $ map (\(msg, key) -> (msg, key, englishScore msg))
        map snd
        $ map (tryKeySize bs)
        $ (take 4) -- try the 4 best key sizes (yay magic numbers)
        $ (rankKeySizes bs maxKeySize)
    where tryKeySize bs keySize =
            (\blocksKeys ->
                -- decoded message, undo the transposition and concatenate.
                (foldr (++) [] $ transpose $ map fst blocksKeys,
                -- the key!
                 map snd blocksKeys))
            -- this gives a list of pairs of blocks & key bytes
            $ map crackXorDecrypt
            $ transpose
            $ (takeBlocks bs keySize)

takeBlocks :: Bytes -> Int -> [Bytes]
takeBlocks [] _ = []
takeBlocks bs blockSize = (take blockSize bs)
    : (takeBlocks (drop blockSize bs) blockSize)

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