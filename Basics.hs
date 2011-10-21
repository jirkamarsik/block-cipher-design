module Basics where

import Char

extractSubblocks :: Int -> Int -> Integer -> [Integer]
extractSubblocks subblockSize subblockCount block =
  let extractSubblocks' 0 block acc = acc
      extractSubblocks' subblocksLeft block acc =
        extractSubblocks' (subblocksLeft - 1) (block `div` 2^subblockSize)
                          (block `mod` 2^subblockSize : acc)
  in extractSubblocks' subblockCount block []

mergeSubblocks :: Int -> Int -> [Integer] -> Integer
mergeSubblocks subblockSize subblockCount subblocks =
  let mergeSubblocks' 0 _ acc = acc
      mergeSubblocks' subblocksLeft [] acc =
        mergeSubblocks' (subblocksLeft - 1) [] (acc * 2^subblockSize)
      mergeSubblocks' subblocksLeft (x:xs) acc =
        mergeSubblocks' (subblocksLeft - 1) xs (acc * 2^subblockSize + x)
   in mergeSubblocks' subblockCount subblocks 0

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

blocksToText :: Int -> [Integer] -> String
blocksToText bytesPerBlock blocks =
  let blocksToText' [] acc = acc
      blocksToText' (b:bs) acc =
        let blockToBytes = extractSubblocks 8 bytesPerBlock
            bytesToText = map (Char.chr . fromIntegral)
            blockToText = bytesToText . blockToBytes
        in blocksToText' bs (reverse (blockToText b) ++ acc)
  in reverse (blocksToText' blocks "")


type KeyedFunction key = key -> Integer -> Integer
type KeySchedule masterKey subkey = masterKey -> [subkey]

iterateCipher :: KeySchedule master sub -> KeyedFunction sub -> Int
                 -> KeyedFunction master
iterateCipher keySchedule roundFunction numIterations masterKey =
  let subkeys = take numIterations $ keySchedule masterKey
      iterations = map roundFunction subkeys
  in foldl (.) id iterations
