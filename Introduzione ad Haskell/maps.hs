-- le mappe applicano una funzione a ogni elemento in una lista

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- i filtri invece selezionano da una lista gli elementi che soddisfano un predicato
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- filtri nel quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

-- funzione takeWhile restituisce la lista finché il predicato vale, appena trova un elem che non soddisfa p, smette
-- es. takeWhile (/= ' ') "elephants know how to party" -- restituisce "elephants"

-- sequenze di Collatz
-- prendiamo un numero naturale, se pari si divide per due, se dispari facciamo *3 + 1, e applichiamo la stessa cosa al risultato,
-- fino a ottenere una catena infinita
-- appena si arriva a 1 la catena però entra in un loop eterno

-- chain :: (Integral a) => a -> [a]
-- chain 1 = [1]
-- chain n
--  | even n = n : chain (n `div` 2)
--  | odd n = n : chain (n * 3 + 1)

-- numLongChains :: Int
-- numLongChains = length (filter isLong (map chain [1 .. 100]))
--  where
--    isLong xs = length xs > 15