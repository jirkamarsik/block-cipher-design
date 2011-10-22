module Main where

import Basics
import Common
import Evaluation
import Data.Hash.MD5
import Data.Bits

md5schedule :: KeySchedule String Integer
md5schedule seed =
  md5i (Str seed) : (md5schedule $ md5s (Str seed))

simpleRoundFunction :: KeyedFunction Integer
simpleRoundFunction key block =
  let [x1,x2,x3,x4,x5,x6,x7,x8] = extractSubblocks 16 8 block
  block `xor` key

stupidCipher :: KeyedFunction String
stupidCipher = iterateCipher md5schedule xor 16

stupidDecipher :: KeyedFunction String
stupidDecipher =
  let keySchedule = \key -> reverse $ take 16 $ md5schedule key
  in iterateCipher keySchedule xor 16

main = do
  text <- readFile "shakespeare-macbeth.txt"
  let cipher = stupidCipher "The secret key!"
  measureDiffusion 16 cipher text
  measureCorrelation 16 cipher text
