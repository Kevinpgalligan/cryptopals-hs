-- Challenge 11: An ECB/CBC detection oracle
-- https://cryptopals.com/sets/2/challenges/11

-- To test, load the module and evaluate this at the REPL...
--  ghci> inferEncryptionMode blackBoxEncrypt
--  "Using CBC!"
--  "Evil spy says..."
--  "  'I think it's using CBC!'"
--  ghci>  

module Cryptopals.Set2.Challenge11 (blackBoxEncrypt, inferEncryptionMode) where

import System.Random (
    getStdGen,
    getStdRandom,
    StdGen,
    random,
    randomR)

import Cryptopals.Util (
    Byte,
    Bytes,
    aes128CbcEncrypt,
    aes128EcbEncrypt,
    aes128BlockSize,
    aes128KeySize,
    padWithPkcsNo7)
import qualified Cryptopals.Util as REPL

minPadding :: Int
minPadding = 5

maxPadding :: Int
maxPadding = 10

randomBytes :: Int -> IO Bytes
randomBytes n = do
    g <- getStdGen
    return (randomBytesFromGen n g)

randomBytesFromGen :: Int -> StdGen -> Bytes
randomBytesFromGen 0 _ = []
randomBytesFromGen n g = (fromIntegral v) : randomBytesFromGen (n-1) nextG
    where (v, nextG) = random g :: (Byte, StdGen)

blackBoxEncrypt :: Bytes -> IO Bytes
blackBoxEncrypt plaintext = do
    let n = length plaintext
    prefixLen <- getStdRandom (randomR (minPadding, maxPadding))
    prefixPad <- randomBytes prefixLen
    postfixLen <- getStdRandom (randomR (minPadding, maxPadding))
    postfixPad <- randomBytes postfixLen
    -- With the random padding they describe in the challenge, it might
    -- not be a multiple of the block size. So using PKCS#7 (from
    -- challenge 9) to add extra padding.
    let toBeEncrypted = padWithPkcsNo7 (fromIntegral aes128BlockSize) (prefixPad ++ plaintext ++ postfixPad)
    key <- randomBytes aes128KeySize
    ebcMode <- (getStdRandom random :: IO Bool)
    if ebcMode
        then do
            print "Using ECB!"
            return (aes128EcbEncrypt key toBeEncrypted)
        else do
            print "Using CBC!"
            iv <- randomBytes aes128BlockSize
            return (aes128CbcEncrypt key iv toBeEncrypted)

inferEncryptionMode :: (Bytes -> IO Bytes) -> IO ()
inferEncryptionMode encryptFn = do
    ciphertext <- encryptFn (take (3 * aes128BlockSize) (repeat 0))
    let secondBlock = take aes128BlockSize (drop aes128BlockSize ciphertext)
    let thirdBlock = take aes128BlockSize (drop (2 * aes128BlockSize) ciphertext)
    print "Evil spy says..."
    -- Compare the output of the 2nd & 3rd blocks (the first one will have the random
    -- padding at the start). Since they're all 0s, the two blocks should be the same
    -- if it's ECB mode. (No influence from previous blocks).
    if secondBlock == thirdBlock
        then print "  'I think it's using ECB!'"
        else print "  'I think it's using CBC!'"