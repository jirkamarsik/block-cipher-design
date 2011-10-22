module Evaluation where

import Basics
import Data.Bits
import Data.Ratio

measureDiffusion :: Int -> (Integer -> Integer) -> String -> IO ()
measureDiffusion blockSize cipher text = do
  let inputBlocks = textToBlocks blockSize text
      computeBitEffects input =
          let output = cipher input
              hammingDist a b =
                length [n | n <- [0..8*blockSize-1], testBit a n /= testBit b n]
          in map (\bit -> let input' = complementBit input bit
                              output' = cipher input'
                          in hammingDist output output') [0..8*blockSize-1]
      bitEffects = map computeBitEffects inputBlocks
      totalBitEffects = foldl (zipWith (+)) (repeat 0) bitEffects
      meanBitEffects =
        map (\x -> (fromIntegral x) / (fromIntegral $ length inputBlocks))
        totalBitEffects
      meanestBitEffect =
        sum meanBitEffects / (fromIntegral $ length meanBitEffects)
      reportMeanBitEffects = sequence $ map (\(x,i) -> putStrLn $
                                "Mean effect of bit " ++ show i ++
                                ": " ++ show x) (zip meanBitEffects [1..])
      reportMeanestBitEffect = putStrLn $ "The mean bit effect: "
                                          ++ show meanestBitEffect
  reportMeanBitEffects
  reportMeanestBitEffect

measureCorrelation :: Int -> (Integer -> Integer) -> String -> IO ()
measureCorrelation blockSize cipher text = do
  let inputBlocks = textToBlocks blockSize text
      outputBlocks = map cipher inputBlocks
      mean xs = sum xs % (fromIntegral $ length xs)
      covar xs ys =
        let x_mean = mean xs
            y_mean = mean ys
        in sum (zipWith (*) (map (\x -> x % 1 - x_mean) xs)
                            (map (\y -> y % 1 - y_mean) ys))
           / (fromIntegral $ length xs - 1)
      var xs = covar xs xs
      correl xs ys =
        let x_stdDev = sqrt $ fromRational $ var xs
            y_stdDev = sqrt $ fromRational $ var ys
        in (fromRational $ covar xs ys) / (x_stdDev * y_stdDev)
      correlation = correl inputBlocks outputBlocks
  putStrLn $ "Pearson product-moment correlation coefficient: "
             ++ (show correlation)
