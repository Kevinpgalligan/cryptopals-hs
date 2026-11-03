-- Challenge 7: AES in ECB mode
-- https://cryptopals.com/sets/1/challenges/7

-- Thanks to this questioner, who stopped me from throwing myself off a bridge:
--  https://stackoverflow.com/questions/42456724/how-to-use-haskell-cryptonite
-- (Also assisted in figuring it out by reading a dystopian, AI-generated slop website, and
--  the crypton source code.)

module Cryptopals.Set1.Challenge07 (solve) where

import qualified Data.ByteString as BS
import System.IO (hClose, hGetContents, openFile, IOMode (ReadMode))

import Cryptopals.Util (aes128EcbDecrypt, decodeBase64, printBytes)

solve :: String -> IO ()
solve filepath = do
    handle <- openFile filepath ReadMode
    content <- hGetContents handle
    let ciphertext = decodeBase64 $ foldr (++) [] (lines content)
    printBytes $ aes128EcbDecrypt ciphertext
    hClose handle