-- "Detect single-character XOR"
-- https://cryptopals.com/sets/1/challenges/4

module Cryptopals.Set1.Challenge04 (solve) where

import System.IO (openFile, hGetContents, IOMode(ReadMode), print, hClose)
import Data.List (minimumBy)
import Data.Ord (comparing)

import Cryptopals.Util (ByteBuffer, englishScore, tryXorDecrypt, hexToBuffer)

solve :: String -> IO ()
solve fileName = do
    handle <- openFile fileName ReadMode
    content <- hGetContents handle
    print (minimumBy (comparing englishScore) $ map (tryXorDecrypt . hexToBuffer) $ lines content)
    hClose handle