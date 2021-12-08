{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
import Data.List (foldl')

main = do
  commands <- map readCommand . lines <$> readFile "../inputs/day02.txt"
  print . (\ (Location d f) -> d*f) . foldl' apply (Location 0 0) $ commands
  print . (\ (Loc2 d f a) -> d*f) . foldl' apply2 (Loc2 0 0 0) $ commands

data Command = Forward Int | Up Int | Down Int

readCommand :: String -> Command
readCommand s = case words s of
                     "forward":[n] -> Forward $ read n
                     "up":[n] -> Up $ read n
                     "down":[n] -> Down $ read n
                     _ -> error "no such format"

data Location = Location { _depth :: Int, _forward :: Int} deriving (Show, Eq)

apply :: Location -> Command -> Location
apply (Location d f) c = case c of
                              Forward n -> Location d (f+n)
                              Up n -> Location (d-n) f
                              Down n -> Location (d+n) f

data Loc2 = Loc2 { _depth :: Int, _forward :: Int, _aim :: Int}

apply2 :: Loc2 -> Command -> Loc2
apply2 (Loc2 d f a) (Down n) = Loc2 d f (a+n)
apply2 (Loc2 d f a) (Up n) = Loc2 d f (a-n)
apply2 (Loc2 d f a) (Forward n) = Loc2 (d + n*a) (f+n) a
