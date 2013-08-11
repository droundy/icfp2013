module Main where

import Ast

main = putStrLn $ show $ length $ take 1000000000 $ enumerate_all_simple 500
