module Storage(
Memory,
readMem,
Id,
allocate,
free,
modify,
newMem,
blockAllocate
)where

import Value
import Definition
import Type
import qualified Data.Map.Strict as M

import Control.Exception.Base

data Memory = Mem (M.Map Id Value) Id deriving Show

newMem = Mem M.empty 1

readMem :: Memory -> Id -> Value
readMem (Mem m _) x = m M.! x

allocate :: Memory -> Value -> (Memory, Id)
allocate (Mem m i) v = (Mem (M.insert i v m) (i+1), i)

blockAllocate :: Memory -> Value -> Int -> (Memory, Id)
blockAllocate (Mem m i) v n = (Mem newMap (i + fromIntegral n), i) where
    newMap = foldl (\dict (key, val) -> M.insert key val dict) m $ zip [i..] (replicate n v)

free :: Memory -> Id -> Memory
free (Mem m i) x = Mem (M.delete x m) i

modify :: Memory -> Id -> (Value -> Value) -> Memory
modify (Mem m i) x f = Mem (M.adjust f x m) i
