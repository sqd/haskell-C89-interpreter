module Utils(
fst',
snd',
span',
extract,
lastN,
race,
isValidIdentifier,
Stack,
top,
pop,
push,
singleton,
updateTop,
while,
(<>>),
getString
)where

import Control.Applicative
import Data.Char

-- Apply a function to the fst element of a pair
fst' :: (a -> b) -> (a, c) -> (b, c)
fst' f (a, b) = (f a, b)

-- Apply a function to the snd element of a pair
snd' :: (a -> b) -> (c, a) -> (c, b)
snd' f (a, b) = (a, f b)

-- Similiar to `span`, but put the first unqualified element in the fst position
-- eg. span (/=';') "abc;efg" = ("abc", ";efg")
-- eg. span' (/=';') "abc;efg" = ("abc;", "efg")
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' f [] = ([], [])
span' f (x:xs) = if f x then fst' (x:) $ span' f xs else ([x], xs)


-- Drop the fst and last elements of a list
extract :: [a] -> [a]
extract = tail . init

-- Find the last n elements of a list
lastN :: Int -> [a] -> [a]
lastN n l = reverse $ take n $ reverse l

-- race a b l: if a appears first in l, returns LT; else GT. If neither, returns EQ
race :: Eq a => a -> a -> [a] -> Ordering
race _ _ [] = EQ
race x y (a:as) =
    if x == y then EQ
    else if x == a then LT
    else if y == a then GT
    else race x y as

isValidIdentifier :: String -> Bool
isValidIdentifier s@(x:xs) = (isAlpha x || x == '_') && (all (\c -> isAlphaNum c || c == '_') xs)

type Stack = []
top = head
pop = tail
push = (:)
-- null
singleton = pure :: a -> Stack a
updateTop f s = push (f $ top s) $ pop s

while con = until (not . con)

(<>>) :: (Monad f, Applicative f) => f (a -> b) -> a -> f b
f <>> x = f <*> return x

getString :: IO String
getString = do
    c <- getChar
    if isSpace c then return "" else (c:) <$> getString
