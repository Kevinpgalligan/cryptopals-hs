-- Challenge 2: Fixed XOR
-- https://cryptopals.com/sets/1/challenges/2
--
-- To solve:
--   λ> (hexToBuffer "1c01110...") `xorBuffers` (hexToBuffer "746865...")
--   "hit the bull's eye"

module Cryptopals.Set1.Challenge02 () where
import Cryptopals.Util (hexToBuffer, xorBuffers)