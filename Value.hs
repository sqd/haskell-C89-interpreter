module Value(
Value(..)
)where

import Definition
import Type

data Value = LVal Id | RVal Type Literal deriving Show

instance Eq Value where
    (LVal a) == (LVal b) = a == b
    (RVal t1 l1) == (RVal t2 l2) = t1 == t2 && l1 == l2
    (LVal _) == (RVal _ _) = error "LVal compared with RVal"

