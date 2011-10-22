module IDEA where

import Basics
import Common
import Data.Bits
import Data.List

idea_mul 0 y = idea_mul (2^16) y
idea_mul x 0 = idea_mul x (2^16)
idea_mul x y = (x * y) `mod` (2^16 + 1)

idea_add x y = (x + y) `mod` (2^16)

idea_group (z1, z2, z3, z4) (x1, x2, x3, x4) =
  (x1 `idea_mul` z1, x2 `idea_add` z2, x3 `idea_add` z3, x4 `idea_mul` z4)

idea_in (z5, z6) (x1, x2, x3, x4) =
  let ma_in1 = x1 `xor` x3
      ma_in2 = x2 `xor` x4
      ma_1 = ma_in1 `idea_mul` z5
      ma_2 = ma_1 `idea_add` ma_in2
      ma_out2 = ma_2 `idea_add` z6
      ma_out1 = ma_1 `idea_add` ma_out2
  in (x1 `xor` ma_out2, x2 `xor` ma_out1, x3 `xor` ma_out2, x4 `xor` ma_out1)

idea_perm (x1, x2, x3, x4) = (x1, x3, x2, x4)

idea_roundFunction :: KeyedFunction [Integer]
idea_roundFunction [z1, z2, z3, z4, z5, z6] =
  (mergeQuartet 64)
  . idea_perm
  . (idea_in (z5, z6))
  . (idea_group (z1, z2, z3, z4))
  . (splitToQuartet 64)

idea_keyScheduleSeries :: Integer -> [Integer]
idea_keyScheduleSeries z =
  extractSubblocks 16 8 z ++ idea_keyScheduleSeries (rotateL z 25)

idea_keyScheduleEncrypt :: Integer -> [[Integer]]
idea_keyScheduleEncrypt z =
  let toRowsOfSix (z1:z2:z3:z4:z5:z6:zs) = [z1,z2,z3,z4,z5,z6] : toRowsOfSix zs
      toRowsOfSix zs = [zs]
  in toRowsOfSix (take 52 $ idea_keyScheduleSeries z)

idea_keyScheduleDecrypt :: Integer -> [[Integer]]
idea_keyScheduleDecrypt z =
  let encryptSchedule = take 52 $ idea_keyScheduleSeries z
      toRowsOfSix (z1:z2:z3:z4:z5:z6:zs) = [z1,z2,z3,z4,z5,z6] : toRowsOfSix zs
      toRowsOfSix [] = []
      keyRows = reverse ((take 4 encryptSchedule)
                         : (toRowsOfSix $ drop 4 encryptSchedule))

      reorderRow [z5,z6,z7,z8,z9,z10] = [z7,z9,z8,z10,z5,z6]
      reorderRow [z1,z2,z3,z4] = [z1,z2,z3,z4]
      add_inv x = 2^16 - x
      mul_inv x =
        let Just inverse = find (\y -> x `idea_mul` y == 1) [0..2^16-1]
        in inverse
      invertRow [z7,z9,z8,z10,z5,z6] = [mul_inv(z7),add_inv(z9),add_inv(z8),
                                        mul_inv(z10),z5,z6]
      invertRow [z1,z2,z3,z4] = [z1,z2,z3,z4]
  in map (invertRow . reorderRow) keyRows

idea :: KeySchedule Integer [Integer] -> KeyedFunction Integer
idea keySchedule z =
  let mainPart = iterateCipher keySchedule idea_roundFunction 8
      [z1, z2, z3, z4] = (keySchedule z) !! 8
      finalKeys = (z1, z2, z3, z4)
  in (mergeQuartet 64)
     . (idea_group finalKeys)
     . idea_perm
     . (splitToQuartet 64)
     . (mainPart z)

idea_encrypt :: KeyedFunction Integer
idea_encrypt = idea idea_keyScheduleEncrypt

idea_decrypt :: KeyedFunction Integer
idea_decrypt = idea idea_keyScheduleDecrypt
