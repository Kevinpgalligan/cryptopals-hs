-- Challenge 8: Detect AES in ECB mode
-- https://cryptopals.com/sets/1/challenges/8

-- TODO
-- See if multiple ciphertexts have repeated blocks.
-- If so, then pick the one with the MOST repeated blocks.

module Cryptopals.Set1.Challenge08 (solve) where

import System.IO (openFile, IOMode (ReadMode), hGetContents, hClose)
import Data.Maybe (fromJust)
import qualified Data.Set as S

import Cryptopals.Util (printBytes, hexToBytes, aes128EcbDecrypt, Bytes)

solve :: String -> IO ()
solve filepath = do
    handle <- openFile filepath ReadMode
    content <- hGetContents handle
    printBytes
        $ aes128EcbDecrypt
        $ fromJust
        $ findEcbCiphertext
        $ map hexToBytes 
        $ lines content
    hClose handle

aesBlockSize :: Int
aesBlockSize = 16

findEcbCiphertext :: [Bytes] -> Maybe Bytes
findEcbCiphertext [] = Nothing
findEcbCiphertext (bs:bss) = if hasRepeatingBlock S.empty bs
                             then Just bs
                             else findEcbCiphertext bss
    where hasRepeatingBlock :: S.Set Bytes -> Bytes -> Bool
          hasRepeatingBlock _ [] = False
          hasRepeatingBlock seenBlocks bs =
            let nextBlock = take aesBlockSize bs
            in if S.member nextBlock seenBlocks
               then True
               else hasRepeatingBlock (S.insert nextBlock seenBlocks) (drop aesBlockSize bs)
