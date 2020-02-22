module Main where

import Lib
import System.Environment

main :: IO ()
main = getArgs >>= parse

parse (fin:fout:[]) = ((writeAnswer . solve . readData) <$> readFile fin) >>= writeFile fout
parse _ = print "Usage: hc20 in.txt out.txt"
