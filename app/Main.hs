module Main where

import Lib

main :: IO ()
main = ((writeAnswer . solve . readData) <$> readFile "in.txt") >>= writeFile "out.txt"

