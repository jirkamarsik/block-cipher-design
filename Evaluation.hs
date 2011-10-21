module Main where

import Basics
import Data.Bits

measureDiffusion :: Int -> (Integer -> Integer) -> IO ()
measureDiffusion blockSize cipher = do
  gutenberg <- readFile "gutenberg.txt"
  let inputBlocks = textToBlocks blockSize gutenberg
      computeBitEffects input =
          let output = cipher input
              hammingDist a b =
                length [n | n <- [0..8*blockSize-1], testBit a n /= testBit b n]
          in map (\bit -> let input' = complementBit input bit
                              output' = cipher input'
                          in hammingDist output output') [0..8*blockSize-1]
      bitEffects = map computeBitEffects inputBlocks
      addLists a b = (map $ uncurry (+)) (zip a b)
      totalBitEffects = foldl addLists (repeat 0) bitEffects
      meanBitEffects =
        map (\x -> (fromIntegral x) / (fromIntegral $ length inputBlocks))
        totalBitEffects
      meanestBitEffect =
        sum meanBitEffects / (fromIntegral $ length meanBitEffects)
      reportMeanBitEffects = sequence $ map (\(i,x) -> putStrLn $
                                "Mean effect of bit " ++ show i ++
                                ": " ++ show x) (zip meanBitEffects [1..])
      reportMeanestBitEffect = putStrLn $ "The mean bit effect: "
                                          ++ show meanestBitEffect
  reportMeanBitEffects
  reportMeanestBitEffect

main = do measureDiffusion 16 id
