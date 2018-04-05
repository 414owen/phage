module Ast 
    ( Ast(..)
    , AstNode(..)
    ) where

newtype Ast = Ast [AstNode]
    deriving (Show)

data AstNode =
      Number Integer
    | Atom String
    | List [AstNode]
    | Bool Bool
    deriving (Show)
