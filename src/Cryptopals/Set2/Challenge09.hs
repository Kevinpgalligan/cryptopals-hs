-- Challenge 9: Implement PKCS#7 padding
-- https://cryptopals.com/sets/2/challenges/9

module Cryptopals.Set2.Challenge09 (padWithPkcsNo7) where

import Data.Word (Word8)

import Cryptopals.Util (Bytes)

padWithPkcsNo7 :: Bytes -> Word8 -> Bytes
padWithPkcsNo7 block blockSize = block ++ (take (fromIntegral fillSize) (repeat fillSize))
    where fillSize = blockSize - fromIntegral ((length block) `mod` (fromIntegral blockSize))