module Tokens
    ( tokenize,
      Token
    ) where

import Data.Char
import Control.Monad (when)


data Token = Plus | Minus | Div | Mult | LeftParenthesis | RightParenthesis | Value Int
  deriving Show

tokenize :: String -> [Token]
tokenize [] = []
tokenize expression = fst clearAndCreateToken : tokenize (snd clearAndCreateToken)
  where
    clearAndCreateToken = createToken $ clearSpace expression

createToken :: String -> (Token, String)
createToken expression = 
  if isDigit(head expression)
  then
    (Value (read [head expression]), tail expression)
  else
    case head expression of
      '+' -> (Plus,             tail expression)
      '*' -> (Mult,             tail expression)
      '/' -> (Div,              tail expression)
      '-' -> (Minus,            tail expression)
      '(' -> (LeftParenthesis,  tail expression)
      ')' -> (RightParenthesis, tail expression)
      _   -> error "Wrong Symbol!"

clearSpace :: String -> String
clearSpace [] = []
clearSpace str = if head str == ' '
                 then clearSpace(tail str)
                 else head str : clearSpace(tail str)
