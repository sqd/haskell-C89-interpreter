module Type(
Type(..)
)where

import Definition

data Type = Polymorphism | TypeAlias Identifier Identifier | TypeArray Type Int | Type Identifier Int deriving (Eq)

instance Show Type where
    show Polymorphism = "?Type?"
    show (Type n i) = n ++ replicate i '*'
