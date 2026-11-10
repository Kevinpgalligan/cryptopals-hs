module Cryptopals.Util
  (Bytes,
  bytesToStr,
  bytesToHex,
  strToBytes,
  printBytes,

  hexValue,
  hexToNum,
  hexToBytes,
  hexDigitVal,

  base64Char,
  numToBase64,
  decodeBase64,
  decodeBase64Char,
  readBase64Blob,

  xorBytes,
  xorWithKey,
  encryptRepeatedKeyXor,

  englishLetterFreq,
  englishAlphabet,
  computeLetterFreq,
  englishScore,
  countNonEnglishChars,
  crackXorEncryption,

  aes128BlockSize,
  aes128EcbDecrypt,
  aes128EcbEncrypt,
  aes128CbcDecrypt,
  aes128CbcEncrypt,
  ) where

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Data.Char (chr, ord, isAlpha, toLower)
import Data.List (elemIndex, sort, group, find, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word8)
import System.IO (readFile)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher(ecbDecrypt, ecbEncrypt), Cipher(cipherInit))
import Crypto.Error (throwCryptoError)

---- Data representation for bytes. ----
type Bytes = [Word8]

-- For pretty printing bytes.
bytesToStr :: Bytes -> String
bytesToStr = T.unpack . TE.decodeLatin1 . BS.pack

strToBytes :: String -> Bytes
-- TODO fromIntegral silently truncates, need error-checking.
strToBytes = map (fromIntegral . ord)

-- 01001101
bytesToHex :: Bytes -> String
bytesToHex [] = []
bytesToHex (b:bs) = let x = 2^4
  in (valueToHex (b `div` x))
      : (valueToHex (b `mod` x))
      : (bytesToHex bs)

printBytes :: Bytes -> IO ()
printBytes = print . bytesToStr

---- Hex conversion -----
hexValue :: Char -> Integer
hexValue c = let o = ord c in
  toInteger (if isAlpha c then 10 + o - ord 'a'
             else o - ord '0')

hexToNum :: String -> Integer
hexToNum = foldl (\sum c -> 16*sum + hexValue c) 0

hexToBytes :: String -> Bytes
-- 2^4=16 hex digits, 2 hex digits make a byte. Pad with 0 if odd number of digits.
hexToBytes s = decodeHexPairs (if odd (length s) then ('0':s) else s)
  where decodeHexPairs [] = []
        decodeHexPairs (a:b:rest) = (16*(hexDigitVal a) + hexDigitVal b) : decodeHexPairs rest

theHexDigits :: String
theHexDigits = "0123456789abcdef"

hexDigitVal :: Char -> Word8
hexDigitVal c = fromIntegral (fromJust (c `elemIndex` theHexDigits))

valueToHex :: Word8 -> Char
valueToHex v =
  if (fromIntegral v) >= (length theHexDigits)
    then (error "Value outside hex range")
  else theHexDigits !! (fromIntegral v)

---- Base64 conversion ----
data Base64Range = Base64Range
  { rangeStart :: Int,
    rangeEnd   :: Int,
    startChar  :: Char
  }

base64Ranges :: [Base64Range]
base64Ranges = [
  Base64Range 0 25 'A',
  Base64Range 26 51 'a',
  Base64Range 52 61 '0',
  Base64Range 62 62 '+',
  Base64Range 63 63 '/'
  ]

base64Char :: Int -> Char
base64Char n = chr ((n - baseNum) + ord baseChar)
  where Base64Range baseNum _ baseChar =
          fromMaybe (error "Invalid base64 index") (find ((n<=).rangeEnd) base64Ranges)

numToBase64 :: Integer -> String
numToBase64 0 = "A"
numToBase64 n = reverse (aux n)
  where aux n = let remainder = quot n 64
                    nextChar  = base64Char (fromInteger (rem n 64))
                in  if remainder > 0
                    then nextChar:(aux remainder)
                    else [nextChar]

decodeBase64 :: String -> Bytes
decodeBase64 [] = []
decodeBase64 cs = (decodeQuad (take 4 cs) 0 0) ++ (decodeBase64 $ drop 4 cs)
  where decodeQuad [] _ _ = []
        decodeQuad ('=':q) nBits value = []
        decodeQuad (c:q) nBits byte =
          let v = decodeBase64Char c
              -- Each base64 character is 6 bits (64=2^6), we can take up
              -- to that many bits to fill up the next byte.
              bitsToTake = (min (8 - nBits) 6)
              bitsLeft = 6 - bitsToTake
              newByte = (byte `shiftL` bitsToTake) `xor` (v `shiftR` bitsLeft)
              newNBits = nBits + bitsToTake
          in if newNBits == 8
             then newByte : (decodeQuad q bitsLeft (v `mod` (1 `shiftL` bitsLeft)))
             else decodeQuad q newNBits newByte

base64Digits :: String
base64Digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

decodeBase64Char :: Char -> Word8
decodeBase64Char = fromIntegral . fromJust . (`elemIndex` base64Digits)

readBase64Blob :: String -> IO Bytes
readBase64Blob filepath = do
  content <- readFile filepath
  let result = decodeBase64 (foldr (++) [] (lines content))
  return result

---- XOR stuff ----
xorBytes :: Bytes -> Bytes -> Bytes
xorBytes = zipWith xor

xorWithKey :: Bytes -> Word8 -> Bytes
xorWithKey bytes key = map (xor key) bytes

---- Frequency analysis ----
englishLetterFractions :: [Double]
englishLetterFractions = [
  0.082, 0.015, 0.028, 0.043, 0.127, 0.022,
  0.02, 0.061, 0.07, 0.0016, 0.0077, 0.04,
  0.024, 0.067, 0.075, 0.019, 0.0012, 0.06,
  0.063, 0.091, 0.028, 0.0098, 0.024, 0.0015,
  0.02, 0.00074
  ]

englishAlphabet :: String
englishAlphabet = "abcdefghijklmnopqrstuvwxyz"

englishLetterFreq :: M.Map Char Double
englishLetterFreq = M.fromList (zip englishAlphabet englishLetterFractions)

computeLetterFreq :: Bytes -> M.Map Char Double
computeLetterFreq bytes =
  M.fromList
  . map (\g -> (head g, fromIntegral (length g) / fromIntegral (length bytes)))
  . group
  . sort
  -- Took me a while to figure this out. We have a [Word8], and we need a [Char].
  -- We convert each Word8 to a Char and also convert to lowercase. `fromIntegral`
  -- is needed because `chr` expects an Int, not a Word8; gotta convert between
  -- integral types.
  . map (toLower . chr . fromIntegral)
  $ bytes

-- Score how likely some bytes are to be English text.
-- Lower score is better.
englishScore :: Bytes -> Double
englishScore bs =
  let letterFreq = computeLetterFreq bs
      getFreq freqMap c = M.findWithDefault 0.0 c freqMap
  -- Compare the frequency distribution, with a penalty for unknown characters.
  in 1.0*(fromIntegral $ countNonEnglishChars bs)
      + (sum
         $ map (\c -> ((getFreq englishLetterFreq c) - (getFreq letterFreq c))^2)
               englishAlphabet)

countNonEnglishChars :: Bytes -> Int
countNonEnglishChars = length
  . (filter (\w8 -> not
              -- May need to expand this in future to include other punctuation.
              $ (\c -> (elem c englishAlphabet) || (c `elem` " .-!,'\""))
              $ toLower
              $ chr
              $ fromIntegral w8))

-- Try all the possible XOR keys, pick the best according to
-- how English-like the results are.
-- Returns the suspected decryption and key byte.
crackXorEncryption :: Bytes -> (Bytes, Word8)
crackXorEncryption bytes = 
  (\(decryption, key, _) -> (decryption, key))
  $ minimumBy (comparing (\(_, _, score) -> score))
  $ map (\k -> let decryption = xorWithKey bytes k
               in (decryption, k, englishScore decryption))
        [1..maxBound]

encryptRepeatedKeyXor :: Bytes -> Bytes -> Bytes
encryptRepeatedKeyXor bytes key = xorBytes bytes (cycleList key)

cycleList :: [a] -> [a]
cycleList [] = []
cycleList xs = loop xs xs
    where loop xs [] = loop xs xs
          loop xs (y:ys) = y:(loop xs ys)

decryptRepeatedKeyXor :: Bytes -> Bytes -> Bytes
-- xor undoes itself, so...
decryptRepeatedKeyXor = encryptRepeatedKeyXor

---- AES ----
aes128BlockSize :: Int
aes128BlockSize = 16

aes128EcbDecrypt :: Bytes -> Bytes -> Bytes
aes128EcbDecrypt key ciphertext = BS.unpack $ ecbDecrypt cipher (BS.pack ciphertext)
    -- Probably very wasteful that we're initialising the cipher every time we call
    -- this function. Especially in the CBC implementation. Should be able to cache
    -- it for efficiency, like in Crypton itself. Oh well.
    where cipher = initAes128Cipher key

aes128EcbEncrypt :: Bytes -> Bytes -> Bytes
aes128EcbEncrypt key plaintext = BS.unpack $ ecbEncrypt cipher (BS.pack plaintext)
  where cipher = initAes128Cipher key

initAes128Cipher :: Bytes -> AES128
initAes128Cipher key = throwCryptoError $ cipherInit (BS.pack key)

aes128CbcEncrypt :: Bytes -> Bytes -> Bytes -> Bytes
aes128CbcEncrypt _ _ [] = []
aes128CbcEncrypt key iv plaintext =
  let cipherblock = aes128EcbEncrypt key (xorBytes iv (take aes128BlockSize plaintext))
  in cipherblock ++ (aes128CbcEncrypt key cipherblock (drop aes128BlockSize plaintext))

aes128CbcDecrypt :: Bytes -> Bytes -> Bytes -> Bytes
aes128CbcDecrypt _ _ [] = []
aes128CbcDecrypt key iv ciphertext =
  let cipherblock = take aes128BlockSize ciphertext
  in (xorBytes iv (aes128EcbDecrypt key cipherblock)) ++ aes128CbcDecrypt key cipherblock (drop aes128BlockSize ciphertext)