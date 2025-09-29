module Tetrafall.Animation (generateIntermediaryScores) where

generateIntermediaryScores :: Int -> Int -> [Int]
generateIntermediaryScores start target
    | start == target = []
    | otherwise = 
        let diff = target - start
            stepSize = fromIntegral diff / 13.0 :: Double
            intermediates = [start + round (stepSize * fromIntegral i) | i <- [1..12 :: Int]]
        in intermediates ++ [target]
