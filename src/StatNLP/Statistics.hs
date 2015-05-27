
module StatNLP.Statistics where


zScore :: Double -> Double -> Double -> Double
zScore m sd x = (x - m) / sd
