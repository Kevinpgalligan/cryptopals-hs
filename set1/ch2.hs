-- "Fixed XOR"
-- https://cryptopals.com/sets/1/challenges/2

-- Representing byte buffers using the ByteString data type. I'd like
-- to abstract that behind a type alias, "ByteBuffer", in case I want
-- to switch out the type later. Not sure yet how to do that.
-- Probably a "type synonym": https://wiki.haskell.org/Type_synonym

import qualified Data.ByteString as B
import Data.Bits (xor)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Word (Word8)

xorBuffers :: B.ByteString -> B.ByteString -> B.ByteString
xorBuffers b1 b2 = B.pack (B.zipWith xor b1 b2)

hexToBuffer :: String -> B.ByteString
-- 2^4 hex digits, 2 hex digits make a byte.
-- Let's say there's an odd number of hex digits. I think it's
-- fair to pad with a zero.
hexToBuffer s = B.pack (hexToBufferAux (if odd (length s) then ('0':s) else s))

hexToBufferAux :: String -> [Word8]
hexToBufferAux [] = []
hexToBufferAux (a:b:rest) = (16*(hexDigitVal a) + hexDigitVal b) : hexToBufferAux rest

hexDigitVal :: Char -> Word8
hexDigitVal c = fromIntegral (fromJust (c `elemIndex` "0123456789abcdef"))

-- Result...
-- λ> (hexToBuffer "1c0111001f010100061a024b53535009181c") `xorBuffers` (hexToBuffer "746865206b696420646f6e277420706c6179")
-- "hit the bull's eye"
