module Main where

import Basics
import Common
import Evaluation
import Data.Hash.MD5
import Data.Bits

md5schedule :: KeySchedule String Integer
md5schedule seed =
  md5i (Str seed) : (md5schedule $ md5s (Str seed))

md5skipSchedule :: KeySchedule String Integer
md5skipSchedule seed =
  map (\x -> let skip = x `mod` 2^6 in (x `div` (2^skip)) `mod` 2^64)
      (md5schedule seed)

macroMA :: KeyedFunction Integer
macroMA key input =
  let keyPair = splitToDuo (feistelBlockBits `div` 2) key
      inputPair = splitToDuo (feistelBlockBits `div` 2) input
      outputPair = maStructure (feistelBlockBits `div` 4) keyPair inputPair
  in mergeDuo (feistelBlockBits `div` 2) outputPair


feistelRounds = 16
feistelBlockBytes = 16
feistelBlockBits = 8 * feistelBlockBytes
feistelFunction = macroMA

simpleFeistelCipher =
  feistelCipher md5schedule feistelRounds feistelBlockBits feistelFunction

simpleFeistelDecipher =
  feistelCipher (\key -> reverse $ take feistelRounds $ md5schedule key)
                feistelRounds feistelBlockBits feistelFunction


main = do
  text <- readFile "blake-poems.txt"
  let cipher = simpleFeistelCipher "The super secret key."
  --measureDiffusion feistelBlockBytes cipher text
  measureDiffusion' feistelBlockBytes cipher text
