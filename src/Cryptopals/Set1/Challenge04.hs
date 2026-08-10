-- Challenge 4: Detect single-character XOR
-- https://cryptopals.com/sets/1/challenges/4

module Cryptopals.Set1.Challenge04 (solve) where

import System.IO (openFile, hGetContents, IOMode(ReadMode), print, hClose)
import Data.List (minimumBy)
import Data.Ord (comparing)

import Cryptopals.Util (bytesToStr, Bytes, englishScore, tryXorDecrypt, hexToBytes)

solve :: String -> IO ()
solve fileName = do
    handle <- openFile fileName ReadMode
    content <- hGetContents handle
    print $ bytesToStr $ (minimumBy (comparing englishScore) $ map (tryXorDecrypt . hexToBytes) $ lines content)
    hClose handle