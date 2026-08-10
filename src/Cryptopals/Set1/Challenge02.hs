-- Challenge 2: Fixed XOR
-- https://cryptopals.com/sets/1/challenges/2
--
-- To solve:
--   λ> bytesToStr $ (hexToBytes "1c01110...") `xorBytes` (hexToBytes "746865...")
--   "hit the bull's eye"

module Cryptopals.Set1.Challenge02 () where
import Cryptopals.Util (hexToBytes, xorBytes, bytesToStr)