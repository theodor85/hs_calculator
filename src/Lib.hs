module Lib
    ( calculator
    ) where

import Tokens ( tokenize, Token )
import Ast ( Ast, buildAst )


calculator :: String -> Int
calculator expression = calculate $ buildAst(tokenize expression)

calculate :: Ast -> Int 
calculate ast = 5 
