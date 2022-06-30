{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Arrow (ArrowChoice (left, right))
import Data.Binary.Builder (singleton)

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- "una lista è o vuota o formata da una testa e una lista"
-- Cons equivale a ":" per creare le liste => 4 (Cons 5 (Cons 6 Empty)) == 4:(5:(6:[]))

-- FINALMENTE ALBERI BINARI
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- quindi un albero o è vuoto, o è un nodo legato a un albero sx e un albero dx
singleton' :: a -> Tree a
singleton' x = Node x EmptyTree EmptyTree -- Albero di un singolo nodo

-- funzione per inserire in un albero un elemento
treeInsert :: (Ord a) => a -> Tree a -> Tree a

treeInstert x EmptyTree = singleton' x

treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

-- funzione per controllare se un elemento è in un albero
-- edge condition -> Albero vuoto, non trovato
-- poi possiamo tenere conto dell'ordinamento "grande - piccolo"
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

data TrafficLight = Red | Yellow | Green

-- class Eq a where
--  (==) :: a -> a -> Bool
--  (/=) :: a -> a -> Bool

-- possiamo renderla istanza di Eq anche "a mano"

-- instance Main.Eq TrafficLight where
-- Red == Red = True
--  Green == Green = True
--  Yellow == Yellow = True
--  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"