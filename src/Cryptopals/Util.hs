module Cryptopals.Util
  (ByteBuffer,

  hexValue,
  hexToNum,
  hexToBuffer,
  hexDigitVal,

  base64Char,
  numToBase64,

  xorBuffers,
  xorWithKey,

  englishLetterFreq,
  englishAlphabet,
  computeLetterFreq,
  englishScore
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Bits (xor)
import Data.Char (chr, ord, isAlpha, toLower)
import Data.List (elemIndex, sort, group, find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word8)

-- Type alias for byte buffers, in case we want to switch out the
-- underlying representation later.
type ByteBuffer = B.ByteString

---- Hex conversion -----
hexValue :: Char -> Integer
hexValue c = let o = ord c in
  toInteger (if isAlpha c then 10 + o - ord 'a'
             else o - ord '0')

hexToNum :: String -> Integer
hexToNum = foldl (\sum c -> 16*sum + hexValue c) 0

hexToBuffer :: String -> ByteBuffer
-- 2^4 hex digits, 2 hex digits make a byte.
-- Let's say there's an odd number of hex digits. I think it's
-- fair to pad with a zero.
hexToBuffer s = B.pack (hexToBufferAux (if odd (length s) then ('0':s) else s))

hexToBufferAux :: String -> [Word8]
hexToBufferAux [] = []
hexToBufferAux (a:b:rest) = (16*(hexDigitVal a) + hexDigitVal b) : hexToBufferAux rest

hexDigitVal :: Char -> Word8
hexDigitVal c = fromIntegral (fromJust (c `elemIndex` "0123456789abcdef"))

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
numToBase64 n = reverse (numToBase64Aux n)

numToBase64Aux :: Integer -> String
numToBase64Aux n =
  let remainder = quot n 64
      nextChar  = base64Char (fromInteger (rem n 64))
  in  if remainder > 0 then nextChar:(numToBase64Aux remainder) else [nextChar]

---- XOR stuff ----
xorBuffers :: ByteBuffer -> ByteBuffer -> ByteBuffer
xorBuffers b1 b2 = B.pack (B.zipWith xor b1 b2)

xorWithKey :: ByteBuffer -> Word8 -> ByteBuffer
xorWithKey buffer key = B.map (xor key) buffer

---- Frequency analysis ----
englishLetterFreq :: [Double]
englishLetterFreq = [
  0.082, 0.015, 0.028, 0.043, 0.127, 0.022,
  0.02, 0.061, 0.07, 0.0016, 0.0077, 0.04,
  0.024, 0.067, 0.075, 0.019, 0.0012, 0.06,
  0.063, 0.091, 0.028, 0.0098, 0.024, 0.0015,
  0.02, 0.00074
]

englishAlphabet :: String
englishAlphabet = "abcdefghijklmnopqrstuvwxyz"

computeLetterFreq :: ByteBuffer -> M.Map Char Double
computeLetterFreq buff =
  M.fromList
  . map (\g -> (head g, fromIntegral (length g) / fromIntegral (B.length buff)))
  . group
  . sort
  -- Took me a while to figure this out. We have a [Word8], and we need a [Char]. We convert
  -- each Word8 to a Char and also convert to lowercase. `fromIntegral` is needed because `chr`
  -- expects an Int, not a Word8; gotta convert between integral types.
  . map (toLower . chr . fromIntegral)
  . B.unpack
  $ buff

-- Score how likely a buffer is to be English text when XORed with a key.
-- Lower score is better.
englishScore :: ByteBuffer -> Word8 -> Double
englishScore buffer key =
  let letterFreq = computeLetterFreq (xorWithKey buffer key)
      -- Unknown characters get a higher score, which is worse.
      getFreq c = M.findWithDefault 1.0 c letterFreq
  in sum [ (getFreq c - f) ^ 2 | (c, f) <- zip englishAlphabet englishLetterFreq ]
