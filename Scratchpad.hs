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

simpleFeistelCipher = feistelCipher md5schedule 8 256 xor

simpleFeistelDecipher =
  feistelCipher (\key -> reverse $ take 8 $ md5schedule key) 8 256 xor

main = do
  text <- readFile "blake-poems.txt"
  let cipher = stupidCipher "The secret key!"
  measureDiffusion 16 cipher text
  measureCorrelation 16 cipher text
