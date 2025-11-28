-- "Single-byte XOR cipher"
-- https://cryptopals.com/sets/1/challenges/3

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word (Word8)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.List (minimumBy, elemIndex, sort, group)
import Data.Char (toLower, chr)
import Data.Maybe (fromJust)

type ByteBuffer = B.ByteString

englishLetterFreq =
  [0.082, 0.015, 0.028, 0.043, 0.127, 0.022,
   0.02, 0.061, 0.07, 0.0016, 0.0077, 0.04,
   0.024, 0.067, 0.075, 0.019, 0.0012, 0.06,
   0.063, 0.091, 0.028, 0.0098, 0.024, 0.0015,
   0.02, 0.00074]
englishAlphabet = "abcdefghijklmnopqrstuvwxyz"

decodeXor :: ByteBuffer -> Word8 -> ByteBuffer
decodeXor buffer key = B.map (xor key) buffer

-- Copied from challenge 2. Need some utility files.
hexToBuffer :: String -> B.ByteString
hexToBuffer s = B.pack (hexToBufferAux (if odd (length s) then ('0':s) else s))
hexToBufferAux :: String -> [Word8]
hexToBufferAux [] = []
hexToBufferAux (a:b:rest) = (16*(hexDigitVal a) + hexDigitVal b) : hexToBufferAux rest
hexDigitVal :: Char -> Word8
hexDigitVal c = fromIntegral (fromJust (c `elemIndex` "0123456789abcdef"))

decrypt :: String -> String
decrypt s =
  let buff = hexToBuffer s
  in minimumBy (comparing (englishScore buff)) [1..(maxBound::Word8)] 

computeLetterFreq :: ByteBuffer -> M.Map Char Double
computeLetterFreq buff = (M.fromList . (map (\g -> (head g, (length g)/(B.length buff)))) . group . sort . (map (toLower . chr)) . B.unpack) buff

englishScore :: ByteBuffer -> Word8 -> Double
englishScore buffer key =
  let letterFreq = (decodeXor buffer key)
  in foldl (\sum c -> sum + getFreq letterFreq c) englishAlphabet

getFreq :: M.Map Char Double -> Char -> Double
getFreq charFreq c = case (M.lookup c charFreq) of
  Just f  -> f
  Nothing -> 0.0
