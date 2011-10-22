module Main where

import Basics
import Common
import Evaluation
import Data.Hash.MD5
import Data.Bits

md5schedule :: KeySchedule String Integer
md5schedule seed =
  md5i (Str seed) : (md5schedule $ md5s (Str seed))

stupidRounds = 15

stupidAddition x y = (x + y) `mod` 2^128

stupidFlip block = 
  let [x1,x2,x3,x4,x5,x6,x7,x8] = extractSubblocks 16 8 block
  in mergeSubblocks 16 8 [x2,x1,x4,x3,x6,x5,x8,x7]

stupidRoundFunction :: KeyedFunction Integer
stupidRoundFunction key = (stupidAddition key) . stupidFlip

stupidCipher :: KeyedFunction String
stupidCipher key =
  stupidFlip . (iterateCipher md5schedule stupidRoundFunction stupidRounds key)

stupidDecipher :: KeyedFunction String
stupidDecipher key =
  let keySchedule = \key -> reverse $ take stupidRounds $ md5schedule key
      inverseKeySchedule = \key -> map (\x -> 2^128 - x) (keySchedule key)
  in stupidFlip
     . (iterateCipher inverseKeySchedule stupidRoundFunction stupidRounds key)


feistelRounds = 16
feistelBlockBytes = 8
feistelBlockBits = 8 * feistelBlockBytes

macroMA :: KeyedFunction Integer
macroMA key input =
  let keyPair = splitToDuo (feistelBlockBits `div` 2) key
      inputPair = splitToDuo (feistelBlockBits `div` 2) input
      outputPair = maStructure (feistelBlockBits `div` 4) keyPair inputPair
  in mergeDuo (feistelBlockBits `div` 2) outputPair

simpleFeistelCipher =
  feistelCipher md5schedule feistelRounds feistelBlockBits macroMA

simpleFeistelDecipher =
  feistelCipher (\key -> reverse $ take feistelRounds $ md5schedule key)
                feistelRounds feistelBlockBits macroMA

main = do
  text <- readFile "blake-poems.txt"
  let cipher = simpleFeistelCipher "The super secret key."
  measureDiffusion feistelBlockBytes cipher text
  measureCorrelation feistelBlockBytes cipher text
