-- "Convert hex to base64"
-- https://cryptopals.com/sets/1/challenges/1

-- This ain't gonna work for larger inputs, need to handle
-- the input string piece-by-piece.
-- To that end...
--   byte   = 2^8
--   hex    = 2^4 -> 2 hex = 1 byte
--   base64 = 2^6 -> 4 base64 = 3 bytes = 6 hex
--            (since lcm(6,8)=24=4 base64=3 bytes)
-- So, the idea would be to take 6 hex characters at a time and
-- convert them into 4 base64 characters. Need to pad with zeros
-- if the number of hex characters isn't divisible by 6. Hm, that's
-- a bit awkward because then we can't distinguish between zeros in
-- the data vs padding. A problem for another day.
-- As it happens, the sample input consists of 96 hex characters, which
-- is divisible by 6.

import Data.Char (chr, ord, isAlpha)
import Data.List (find)
import Data.Maybe (fromMaybe)

hexValue :: Char -> Integer
hexValue c = let o = ord c in
  toInteger (if isAlpha c then 10 + o - ord 'a'
             else o - ord '0')

hexToNum :: String -> Integer
hexToNum = foldl (\sum c -> 16*sum + hexValue c) 0

data Base64Range = Base64Range
  { rangeStart :: Int,
    rangeEnd   :: Int,
    startChar  :: Char }

base64Ranges = [
  Base64Range 0 25 'A',
  Base64Range 26 51 'a',
  Base64Range 52 61 '0',
  Base64Range 62 62 '+',
  Base64Range 63 63 '/']

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

hexToBase64 :: String -> String
hexToBase64 = numToBase64 . hexToNum
