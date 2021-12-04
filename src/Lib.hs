module Lib
    ( calculator
    ) where

import Tokens


calculator :: String -> Int
calculator expression = calculate $ buildAst(tokenize expression)

buildAst :: [Token] -> Ast
buildAst tokens = Ast 5

newtype Ast = Ast Int

calculate :: Ast -> Int 
calculate ast = 5 
