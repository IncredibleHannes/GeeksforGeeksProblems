-- Simple approach for the problem:
-- http://www.geeksforgeeks.org/find-median-row-wise-sorted-matrix/

list :: [[Int]]
list = [[1,3,5],
        [1,6,8],
        [4,7,9]]

sort :: [[Int]] -> [Int]
sort []     = []
sort [x]    = x
sort (x:xs) = sort $ reverse (merge x (head xs) []) : tail xs
  where
    merge [] [] e         = e
    merge [] y e          = (reverse y) ++ e
    merge x [] e          = (reverse x) ++ e
    merge (x:xs) (y:ys) e = if x < y
                            then merge xs (y:ys) (x : e)
                            else merge (x:xs) ys (y : e)

getMedian :: [Int] -> Double
getMedian x | odd $ length x = fromIntegral (x !! (length x `div` 2)) / 1
            | otherwise      = fromIntegral ((x !! ((length x) `div` 2)) +
                                             (x !! (((length x) `div` 2) - 1))) / 2
