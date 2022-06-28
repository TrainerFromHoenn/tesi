-- Le "high order functions" sono funzioni di Haskell che utilizzano funzioni come parametri
-- Queste funzioni sono il cuore del funzionamento di Haskell

-- CURRIED functions
-- Le funzioni normalmente in Haskell accettano SOLO un parametro, ma come fanno le funzioni con 2 param a funzionare?
-- Vengono automaticamente riscritte
-- max 4 5 -> crea una funzione che prima applica max a 4, poi riapplica al risultato 5
-- (max 4) 5

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 -- Restituisce una funzione che compara un altro input con 100

-- esempio con le funzioni infisse
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10) -- ecco perché posso scrivere 200 / 10, e il "/" pur essendo una funzione sta al centro

-- Le funzioni non sono istanze della typeclass Show, quindi ocio
-- per esempio se faccio 1 + 1, ghci calcola 2 e poi applica la funzione show su 2

-- funzioni che prendono funzioni e fanno altro
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x) -- essenzialmente è "f(f(x))"

-- Implementiamo una delle funzioni più importanti della libreria standard
-- Prende 2 liste e le joina applicando una funzione fra gli elementi corrispondenti
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] -- PARAMETRI: 1 funzione con 2 input, e poi 2 input liste
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- es. zipWith' (+) [4,2,5,6] [2,6,2,3] => [6,8,7,9]

flip' :: (a -> b -> c) -> (b -> a -> c) -- Scambia gli input di una funzione
flip' f y x = f x y
