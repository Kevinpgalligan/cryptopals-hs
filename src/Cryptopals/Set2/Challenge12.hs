-- Challenge 12: Byte-at-a-time ECB decryption (Simple)
-- https://cryptopals.com/sets/2/challenges/12

-- To test:
--   ghci> bytesToStr $ decryptByteAtATime secretEncrypt
-- (Note: I didn't bother implementing the check for ECB mode, i.e. step 2).

module Cryptopals.Set2.Challenge12 (
    secretEncrypt,
    decryptByteAtATime,
    deriveBlockSize,
    lastN,
    inferByte,
    deriveSecretSize) where

import Cryptopals.Util (
    Bytes,
    Byte,
    strToBytes,
    aes128EcbEncrypt,
    padWithPkcsNo7,
    decodeBase64,
    aes128BlockSize)

secretMsgAsBase64 = [
    "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg",
    "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq",
    "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg",
    "YnkK"]

secretEncrypt :: Bytes -> Bytes
secretEncrypt plaintext = aes128EcbEncrypt fixedKey input
    where fixedKey = strToBytes "YELLOW SUBMARINE"
          secretMsg = decodeBase64 $ foldr (++) [] secretMsgAsBase64
          input = (padWithPkcsNo7 aes128BlockSize) $ (plaintext ++ secretMsg)

decryptByteAtATime :: (Bytes -> Bytes) -> Bytes
decryptByteAtATime encryptFn = aux 0 []
    where blockSize = deriveBlockSize encryptFn
          secretSize = deriveSecretSize encryptFn
          aux byteIndex result =
            if byteIndex >= secretSize
                then result
                else aux (byteIndex+1)
                         (result
                          ++ [decryptByte encryptFn blockSize byteIndex result])

decryptByte :: (Bytes -> Bytes) -> Int -> Int -> Bytes -> Byte
decryptByte encryptFn blockSize byteIndex partialResult
    -- Need to pad the input to the encryption function so that
    -- the next target byte is the last byte in a block (such
    -- that we know all the other bytes in the block).
    = let padSize = 15 - (byteIndex `mod` blockSize)
          shiftedIndex = byteIndex + padSize
          blocksToDrop = shiftedIndex `div` blockSize
          knownBytesInBlock = lastN (blockSize-1) partialResult
          pad = take padSize (repeat 0)
          ciphertext = encryptFn pad
      -- Now take the block that the target byte was encrypted in. We
      -- know all the other bytes in that block, so we can infer the
      -- target byte by trying all the possible values.
      in inferByte encryptFn
                   (take blockSize (drop (blocksToDrop*blockSize) ciphertext))
                   -- If we know BlockSize-1 or more bytes already,
                   -- then the target block is made up of the last
                   -- BlockSize-1 of those bytes, plus the unknown byte.
                   -- If we don't know that many bytes, we need to add the zero
                   -- padding (up to BlockSize-1 bytes).
                   ((take (max 0 ((blockSize-1) - (length knownBytesInBlock)))
                          (repeat 0))
                    ++ knownBytesInBlock)
                   blockSize

lastN :: Int -> [a] -> [a]
lastN n xs = aux xs (drop n xs)
    where aux xs [] = xs
          aux (x:xs) (y:ys) = aux xs ys

inferByte :: (Bytes -> Bytes) -> Bytes -> Bytes -> Int -> Byte
inferByte encryptFn cipherblock knownBytes blockSize =
    head
    $ filter (\v -> cipherblock == take blockSize (encryptFn (knownBytes ++ [v])))
    -- Could order so that alphabetic letters & other common
    -- characters come first, for efficiency. But that's more effort.
    $ [0..maxBound]

deriveBlockSize :: (Bytes -> Bytes) -> Int
-- Try increasingly bigger block sizes until we find one that's different from
-- the block size we'd get with just 1 byte. Then get the difference between them.
deriveBlockSize encryptFn = nextBiggestSize - minSize
    where minSize = length (encryptFn [0])
          nextBiggestSize =
            head
            $ filter ((/=) minSize)
            $ map (\inputSize -> length $ encryptFn (take inputSize $ repeat 0))
                  [2..]

deriveSecretSize :: (Bytes -> Bytes) -> Int
-- Similar to how we derive the block size, except we track how many bytes we have to
-- add. If we have to input N bytes to change the output message size, then there must
-- be N-1 bytes of padding, and the secret message must be MinOutputSize-(N-1) bytes.
deriveSecretSize encryptFn = minSize - (bytesNeededToChangeOutputSize - 1)
    where minSize = length (encryptFn [])
          bytesNeededToChangeOutputSize =
            head
            $ filter (\numBytes -> minSize /= (length $ encryptFn (take numBytes (repeat 0))))
                     [1..]