{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Data.List (foldl')

main = do
  nums :: [Int] <- map read . lines <$> readFile "../inputs/day01.txt"
  print $ countIncs nums
  print $ partTwo nums


countIncs :: (Ord a) => [a]-> Int
countIncs [] = 0
countIncs (a:r) = snd $ foldl' f (a,0) r
  where
    f (l,count) h = if h > l then (h, count+1) else (h, count)


slideThree l@(a:r1@(b:r2)) = zipWith3 (\x y z -> x+y+z) l r1 r2

partTwo = countIncs . slideThree
