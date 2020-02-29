module Main where

import Lib as Lib
import Lob as Lob
import System.Environment

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse (fin:[]) = ((show . Lob.readData) <$> readFile fin) >>= print
-- parse (fin:fout:[]) = ((Lib.writeAnswer . Lib.solve . Lib.readData) <$> readFile fin) >>= writeFile fout
parse (fin:fout:[]) = ((Lob.writeAnswer . Lob.solve . Lob.readData) <$> readFile fin) >>= writeFile fout
parse _ = print "Usage: hc20 in.txt out.txt"
