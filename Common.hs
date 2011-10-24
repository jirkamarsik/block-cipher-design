module Common where

import Basics
import Data.Bits

-- The following four are convenience functions for packing and unpacking
-- small numbers into/from large numbers.

splitToDuo :: Int -> Integer -> (Integer, Integer)
splitToDuo blockSize block =
  let [left, right] = extractSubblocks (ceiling (fromIntegral blockSize / 2)) 2
                                       block
  in (left, right)

mergeDuo :: Int -> (Integer, Integer) -> Integer
mergeDuo blockSize (left, right) =
  mergeSubblocks (ceiling (fromIntegral blockSize / 2)) 2 [left, right]

splitToQuartet :: Int -> Integer -> (Integer, Integer, Integer, Integer)
splitToQuartet blockSize block =
  let [x1, x2, x3, x4] = extractSubblocks (ceiling (fromIntegral blockSize / 4))
                                          4 block
  in (x1, x2, x3, x4)

mergeQuartet :: Int -> (Integer, Integer, Integer, Integer) -> Integer
mergeQuartet blockSize (x1,x2,x3,x4) =
  mergeSubblocks (ceiling (fromIntegral blockSize / 4)) 4 [x1, x2, x3, x4]


-- Constructing the round function of a Feistel cipher given the function F.
feistelRoundFunction :: Int -> KeyedFunction key -> KeyedFunction key
feistelRoundFunction blockSize embeddedFunction key block =
  let (left, right) = splitToDuo blockSize block
      f_right = embeddedFunction key right
  in mergeDuo blockSize (right, left `xor` f_right)

-- A schema for Feistel ciphers, a very simple and elegant way of constructing
-- trapdoor functions from cryptographically weaker functions.
feistelCipher :: KeySchedule masterKey subkey -> Int -> Int
              -> KeyedFunction subkey -> KeyedFunction masterKey
feistelCipher keySchedule numIterations blockSize embeddedFunction key block =
  let theNetwork =
        iterateCipher keySchedule
                      (feistelRoundFunction blockSize embeddedFunction)
                      numIterations
      undoLastSwap = (\b -> let (l,r) = splitToDuo blockSize b
                            in mergeDuo blockSize (r,l))
  in undoLastSwap (theNetwork key block)


-- The round function in the Lai-Massey schema.
laiMasseyRoundFunction :: Int -> KeyedFunction key
                       -> (Integer -> Integer -> Integer)
                       -> (Integer -> Integer) -> KeyedFunction key
laiMasseyRoundFunction blockSize embeddedFunction groupAdd groupInv key block =
  let (left, right) = splitToDuo blockSize block
      f_input = left `groupAdd` (groupInv right)
      f_output = embeddedFunction key f_input
  in mergeDuo blockSize (left `groupAdd` f_output, right `groupAdd` f_output)

-- The general Lai-Massey schema for ciphers.
laiMasseyCipher :: KeySchedule masterKey subkey -> Int -> Int
                -> KeyedFunction subkey -> (Integer -> Integer -> Integer)
                -> (Integer -> Integer) -> KeyedFunction masterKey
laiMasseyCipher keySchedule numIterations blockSize embeddedFunction
                groupAdd groupInv =
  iterateCipher
    keySchedule
    (laiMasseyRoundFunction blockSize embeddedFunction groupAdd groupInv)
    numIterations


-- The MA structure which ended up in our cipher.
maStructure :: Int -> (Integer, Integer) -> (Integer, Integer)
            -> (Integer, Integer)
maStructure modulus (key1, key2) (in1, in2) =
  let add x y = (x + y) `mod` 2^modulus
      mul 0 y = (2^modulus) `mul` y
      mul x 0 = x `mul` (2^modulus)
      mul x y = (x * y) `mod` (2^modulus + 1)
      a = in1 `mul` key1
      b = a `add` in2
      c = b `mul` key2
      d = c `add` a
  in (c, d)
