module Main where
-- This module was our workbench and now contains the resulting cipher.

import Basics
import Common
import Evaluation
import Data.Hash.MD5
import Data.Bits

-- A 128-bit key schedule of successive MD5 hashes. This forms the basis
-- of our key schedule. The first subkey is the MD5 hash of the master key
-- string, further subkeys are obtained by taking the MD5 hash of the string
-- representation of the previous subkey's MD5 hash.
md5schedule :: KeySchedule String Integer
md5schedule seed =
  md5i (Str seed) : (md5schedule $ md5s (Str seed))

-- The 64-bit key schedule used by our cipher. Every subkey of the above key
-- schedule is examined. Based on its last 6 bits, a 64-bit continuous
-- subsequence of it is taken as the subkey.
md5skipSchedule :: KeySchedule String Integer
md5skipSchedule seed =
  map (\x -> let skip = x `mod` 2^6 in (x `div` (2^skip)) `mod` 2^64)
      (md5schedule seed)

-- A wrapper for the MA structure. Splits the two input and key 2k-words
-- into pairs of k-words and feeds them to the MA structure. The resulting pair
-- of k-words are assembled into a 2k-word.
macroMA :: KeyedFunction Integer
macroMA key input =
  let keyPair = splitToDuo (feistelBlockBits `div` 2) key
      inputPair = splitToDuo (feistelBlockBits `div` 2) input
      outputPair = maStructure (feistelBlockBits `div` 4) keyPair inputPair
  in mergeDuo (feistelBlockBits `div` 2) outputPair

-- Our cipher uses 16 rounds of iteration. A tentative safe value which might
-- be easily adjusted (perhaps 8 might be fine as well).
feistelRounds = 16
-- The block size is 16 bytes = 128 bits.
feistelBlockBytes = 16
feistelBlockBits = 8 * feistelBlockBytes
-- The function F embedded in the Feistel network,
-- in our case, the MA structure.
feistelFunction = macroMA

-- Our cipher. Uses the Feistel network layout, the MD5-skip key schedule,
-- MA structure as the Feistel function and 16 rounds of iteration.
simpleFeistelCipher =
  feistelCipher md5skipSchedule feistelRounds feistelBlockBits feistelFunction

-- This is the decrypter. The only difference from the encrypter is that
-- the decrypter uses a reversed key schedule.
simpleFeistelDecipher =
  feistelCipher (\key -> reverse $ take feistelRounds $ md5skipSchedule key)
                feistelRounds feistelBlockBits feistelFunction

-- Simple wrappers for the desired functionality of our cipher.
encode key = (map (simpleFeistelCipher key)) . (textToBlocks 16)
decode key = (blocksToText 16) . (map (simpleFeistelDecipher key))

-- We used this place to compile and run some basic analysis of the cipher.
-- First and foremost we measured the effect of changing individual bits of
-- the input on the bits of the output (see Evaluation.hs).
main = do
  text <- readFile "blake-poems.txt"
  let cipher = simpleFeistelCipher "This is the secret key."
  --measureDiffusion feistelBlockBytes cipher text
  measureDiffusion' feistelBlockBytes cipher text
