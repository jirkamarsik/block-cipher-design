module Evaluation where
-- A module where we implement some analyses of ciphers to provide us
-- feedback during our design.

import Basics
import Data.Bits
import Data.Ratio

-- Report the average number of outputs changed when individual input bits
-- are inverted.
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

-- Report the estimated probabilities of an output bit changing when a bit
-- in the input is inverted.
measureDiffusion' :: Int -> (Integer -> Integer) -> String -> IO ()
measureDiffusion' blockSize cipher text = do
  let inputBlocks = textToBlocks blockSize text
      computeChangedBits input =
          let output = cipher input
              differenceBits a b =
                [n | n <- [0..8*blockSize-1], testBit a n /= testBit b n]
          in concat $ map (\bit -> let input' = complementBit input bit
                                       output' = cipher input'
                                   in differenceBits output output')
                          [0..8*blockSize-1]
      changedBits = concat $ map computeChangedBits inputBlocks
      probChange bit = (/ fromIntegral (8*blockSize * length inputBlocks))
                       $ fromIntegral $ length $ filter (== bit) changedBits
      reportBitChangeProbs =
        sequence $ map (\i -> putStrLn $ "Probability of bit " ++ show i ++
                                         " changing = " ++ show (probChange i))
                       [0..8*blockSize-1]
  _ <- reportBitChangeProbs
  return ()

-- We used Haskell's arbitrary-length integer arithmetic to implement
-- the computation of Pearson's product-moment correlation coefficient.
-- As was expected, the metric didn't really apply well to the highly sparse
-- and non-linear data samples from cryptographic functions.
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
