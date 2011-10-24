module Basics where

import Char

-- The fundamental type declarations. A keyed function is something, which
-- produces a function given a key. In our implementation, or binary blocks are
-- represented as large numbers (the Integer type in Haskell).
type KeyedFunction key = key -> Integer -> Integer
-- A key schedule is just something capable of producing a series of subkeys
-- given a master key.
type KeySchedule masterKey subkey = masterKey -> [subkey]

-- A simple schema for iterated ciphers. It is enough to just specify the key
-- schedule, the keyed round function and the number of iterations and you get
-- the resulting keyed cipher.
iterateCipher :: KeySchedule master sub -> KeyedFunction sub -> Int
              -> KeyedFunction master
iterateCipher keySchedule roundFunction numIterations masterKey =
  let subkeys = take numIterations $ keySchedule masterKey
      iterations = map roundFunction subkeys
  in foldl (.) id iterations


-- Takes a large number (e.g. AAABBBCCCDDD) and splits it into smaller ones
-- (AAA, BBB, CCC and DDD).
extractSubblocks :: Int -> Int -> Integer -> [Integer]
extractSubblocks subblockSize subblockCount block =
  let extractSubblocks' 0 block acc = acc
      extractSubblocks' subblocksLeft block acc =
        extractSubblocks' (subblocksLeft - 1) (block `div` 2^subblockSize)
                          (block `mod` 2^subblockSize : acc)
  in extractSubblocks' subblockCount block []

-- Takes several numbers and concatenates them into a larger number.
mergeSubblocks :: Int -> Int -> [Integer] -> Integer
mergeSubblocks subblockSize subblockCount subblocks =
  let mergeSubblocks' 0 _ acc = acc
      mergeSubblocks' subblocksLeft [] acc =
        mergeSubblocks' (subblocksLeft - 1) [] (acc * 2^subblockSize)
      mergeSubblocks' subblocksLeft (x:xs) acc =
        mergeSubblocks' (subblocksLeft - 1) xs (acc * 2^subblockSize + x)
   in mergeSubblocks' subblockCount subblocks 0

-- The conversion of a textual string into a list of binary blocks (represented
-- as Haskell's large numbers). Characters are assumed to have only one byte
-- codepoints, non-Latin1 characters are removed.
textToBlocks :: Int -> String -> [Integer]
textToBlocks bytesPerBlock text =
  let textToBlocks' [] acc = acc
      textToBlocks' text acc =
        let textToBytes = (map (fromIntegral . Char.ord))
                        . (filter Char.isLatin1)
            bytesToBlock = mergeSubblocks 8 bytesPerBlock
            textToBlock = bytesToBlock . textToBytes
        in  textToBlocks' (drop bytesPerBlock text) ((textToBlock text) : acc)
  in reverse (textToBlocks' text [])

-- The conversion of a list of binary blocks back into strings of one-byte
-- characters.
blocksToText :: Int -> [Integer] -> String
blocksToText bytesPerBlock blocks =
  let blocksToText' [] acc = acc
      blocksToText' (b:bs) acc =
        let blockToBytes = extractSubblocks 8 bytesPerBlock
            bytesToText = map (Char.chr . fromIntegral)
            blockToText = bytesToText . blockToBytes
        in blocksToText' bs (reverse (blockToText b) ++ acc)
  in reverse (blocksToText' blocks "")
