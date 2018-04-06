module Ast 
    ( Ast(..)
    , AstNode(..)
    ) where

newtype Ast = Ast [AstNode]
    deriving (Show)

data AstNode
    = ANum Integer
    | AAtom String
    | AList [AstNode]
    deriving (Show)
