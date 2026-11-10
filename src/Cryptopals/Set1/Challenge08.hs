-- Challenge 8: Detect AES in ECB mode
-- https://cryptopals.com/sets/1/challenges/8

module Cryptopals.Set1.Challenge08 (solve) where

import System.IO (openFile, IOMode (ReadMode), hGetContents, hClose)
import Data.Maybe (fromJust)
import qualified Data.Set as S

import Cryptopals.Util (printBytes, hexToBytes, Bytes, bytesToHex)

solve :: String -> IO ()
solve filepath = do
    handle <- openFile filepath ReadMode
    content <- hGetContents handle
    print
        -- Trying to decrypt with the hardcoded key, "YELLOW SUBMARINE", gave nonsense. So apparently
        -- we're just supposed to print the hex.
        -- (I compared to other solutions, and they seemed to do the same thing. I also confirmed that
        --  only one of the ciphertexts has repeat blocks.)
        $ bytesToHex
        $ fromJust
        $ findEcbCiphertext
        $ map hexToBytes 
        $ lines content
    hClose handle

aesBlockSize :: Int
aesBlockSize = 16

findEcbCiphertext :: [Bytes] -> Maybe Bytes
findEcbCiphertext [] = Nothing
findEcbCiphertext (bs:bss) =
    if (countRepeatBlocks bs) > 0
    then Just bs
    else findEcbCiphertext bss

countRepeatBlocks :: Bytes -> Int
countRepeatBlocks bs = recur bs S.empty 0
    where recur :: Bytes -> S.Set Bytes -> Int -> Int
          recur [] _ count = count
          recur bs seenBlocks count =
            let nextBlock = take aesBlockSize bs
                rest = drop aesBlockSize bs
                alreadySeen = S.member nextBlock seenBlocks
            in if alreadySeen
               then recur rest seenBlocks (count+1)
               else recur rest (S.insert nextBlock seenBlocks) count