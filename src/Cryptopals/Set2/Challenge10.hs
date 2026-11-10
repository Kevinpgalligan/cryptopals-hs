-- Challenge 10: Implement CBC mode
-- https://cryptopals.com/sets/2/challenges/10

module Cryptopals.Set2.Challenge10 (solve) where

import Cryptopals.Util (aes128CbcDecrypt, readBase64Blob, printBytes, strToBytes, aes128BlockSize)

solve :: String -> IO ()
solve filepath = do
    ciphertext <- readBase64Blob filepath
    printBytes $ aes128CbcDecrypt (strToBytes "YELLOW SUBMARINE") (take aes128BlockSize (repeat 0)) ciphertext