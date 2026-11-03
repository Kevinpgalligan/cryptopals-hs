-- Challenge 7: AES in ECB mode
-- https://cryptopals.com/sets/1/challenges/7

-- Thanks to this questioner, who stopped me from throwing myself off a bridge:
--  https://stackoverflow.com/questions/42456724/how-to-use-haskell-cryptonite
-- (Also assisted in figuring it out by reading a dystopian, AI-generated slop website, and
--  the crypton source code.)

module Cryptopals.Set1.Challenge07 (solve) where

import qualified Data.ByteString as BS
import System.IO (hClose, hGetContents, openFile, IOMode (ReadMode))

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..))
import Crypto.Error (throwCryptoError)

import Cryptopals.Util (strToBytes, decodeBase64)

solve :: String -> IO ()
solve filepath = do
    handle <- openFile filepath ReadMode
    content <- hGetContents handle
    let ciphertext = BS.pack $ decodeBase64 $ foldr (++) [] (lines content)
    print $ aes128EcbDecrypt ciphertext
    hClose handle

-- Will probably want to revise the interface of this for general use. Do we want to work with
-- bytestrings? That's what crypton accepts. But that's obviously incompatible with our Bytes type.
-- Now maybe regretting inventing a new type, woops.
-- Also, probably don't want to hardcode the key as "YELLOW SUBMARINE".
aes128EcbDecrypt :: BS.ByteString -> BS.ByteString
aes128EcbDecrypt ciphertext = ecbDecrypt cipher ciphertext
    where cipher :: AES128
          cipher = throwCryptoError $ cipherInit (BS.pack $ strToBytes "YELLOW SUBMARINE")