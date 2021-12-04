module Ast
    ( buildAst,
      Ast
    ) where

import Tokens     ( Token )
import BinaryTree ( BinaryTree )


newtype Ast = BinaryTree Token

buildAst :: [Token] -> Ast
buildAst []               = EmptyTree
buildAst [token : tokens] = insertItem (buildAst tokens) token   


insertItem :: Ast -> Token -> Ast 
insertItem EmptyNode token         = Branch token EmptyNode EmptyNode
insertItem (Branch token left right) new_token = 
  case new_token of
    Value Int    -> Leaf new_token
    Plus | Minus -> Branch new_token  
    | b < a = Branch a (insertItem left b) right
    | b > a = Branch a left (insertItem right b)

