-- http://www.geeksforgeeks.org/evil-number/

intToBinary :: Int -> [Int]
intToBinary x | x > 0     = (x `mod` 2) : intToBinary (x `div` 2)
              | otherwise = []

isEvilNumber :: Int -> Bool
isEvilNumber x = even $ length $ filter (\x -> x > 0) (intToBinary x)
