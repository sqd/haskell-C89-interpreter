module Runtime(
typeCast,
defaultVal,
toBool,
boolType,
true,
false,
isTrue,
isFalse,
voidType,
void,
intType,
makePtr
)where

import Grammar
import Parse
import Type
import Value
import Control.Applicative

-- Input: Target type, a RIGHT value
typeCast :: Type -> Value -> Value
typeCast t1 v@(RVal t2 x)
    | t1 == t1 = v
typeCast (Type "bool" 0) (RVal _ x) = if x == "0" then false else true
typeCast (Type "int" 0) (RVal Polymorphism x) = RVal intType $ show $ (read x :: Integer)
typeCast Polymorphism (RVal _ x) = RVal Polymorphism x
typeCast x v = error $ "Type Cast error: " ++ show x ++ " | " ++ show v

defaultVal :: Type -> Value
defaultVal (Type "bool" 0) = false
defaultVal (Type "int" 0) = RVal intType "0"

toBool = typeCast boolType
boolType = Type "bool" 0
true = RVal boolType "true"
false = RVal boolType "false"
isTrue = (== true) . toBool
isFalse = not . isTrue

voidType = Type "void" 0
void = RVal voidType ""

intType = Type "int" 0
makeInt = RVal intType . (show :: Integer -> String)

makePtr = Type
