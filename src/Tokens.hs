module Tokens
    ( tokenize,
      Token
    ) where

import Data.Char ( isDigit )


data Token = Plus | Minus | Div | Mult | LeftParenthesis | RightParenthesis | Value Int
  deriving Show

tokenize :: String -> [Token]
tokenize []         = []
tokenize expression = fst clearAndCreateToken : tokenize (snd clearAndCreateToken)
  where
    clearAndCreateToken = createToken $ clearSpace expression

createToken :: String -> (Token, String)
createToken expression =
  let exp_head = head expression
      exp_tail = tail expression
  in
    if isDigit(head expression)
    then
      createValue expression
    else
      case head expression of
        '+' -> (Plus,             exp_tail)
        '*' -> (Mult,             exp_tail)
        '/' -> (Div,              exp_tail)
        '-' -> (Minus,            exp_tail)
        '(' -> (LeftParenthesis,  exp_tail)
        ')' -> (RightParenthesis, exp_tail)
        _   -> error "Wrong Symbol!"

createValue :: String -> (Token, String)
createValue exp =
  let (rest_exp, digits) = getDigits exp ""
  in
    (Value (read digits), rest_exp)

getDigits :: String -> String -> (String, String)
getDigits ""  result = ("", result)
getDigits str result =
  if isDigit (head str)
  then
    getDigits (tail str) (result ++ [head str])
  else
    (str, result)

clearSpace :: String -> String
clearSpace []  = []
clearSpace str = if head str == ' '
                 then clearSpace(tail str)
                 else head str : clearSpace(tail str)
  