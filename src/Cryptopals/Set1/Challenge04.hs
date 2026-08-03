-- "Detect single-character XOR"
-- https://cryptopals.com/sets/1/challenges/4

module Cryptopals.Set1.Challenge04 (solve) where

import System.IO (openFile, hGetContents, IOMode(ReadMode), print, hClose)
import Data.List (minimumBy)
import Data.Ord (comparing)

import Cryptopals.Util (ByteBuffer, englishScore, tryXorDecrypt, hexToBuffer)

-- TODO: possible issue is that we're just going through the English alphabet, the
--       penalisation is not being applied. So need to loop through the list of decrypted
--       characters and compare to their known English frequencies (or penalty if they're an
--       invalid character)
solve :: String -> IO ()
solve fileName = do
    handle <- openFile fileName ReadMode
    content <- hGetContents handle
    print (minimumBy (comparing englishScore) $ map (tryXorDecrypt . hexToBuffer) $ lines content)
    hClose handle