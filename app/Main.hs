module Main where

import Lib

main :: IO ()
main = do  
    putStrLn "Enter an expression:"  
    expression <- getLine  
    putStrLn ("Result: " ++ show (calculator expression))  
