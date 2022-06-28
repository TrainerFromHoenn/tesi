-- un pattern comune è rappresentare le liste come (x:xs), fare qualcosa su x e ripetere su xs
-- i fold semplificano questo processo, prendono una funzione, un valore iniziale (accumulatore) e una lista

-- FOLDL

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- La funzione è \acc x -> acc + x, 0 è il valore di partenza e xs la lista da "ripiegare"
-- Versione migliorata:

-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (+) 0
-- Altra funzione con left fold
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else False) False ys -- In questo caso False viene usato come starting value

-- FOLDR

-- con il foldr l'accumulatore "consuma" i valori da destra
-- si passa da \acc x -> a \x acc

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- R e L sono intercambiabili con perdita di leggibilità
-- Il vantaggio della R è che è funziona con le liste infinite

-- foldl1 e foldr1 sono le stess funzioni ma senza il valore iniziale (decide runtime in base alla lista che si ritrova)
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- SCANL E SCANR
-- come foldr e foldl, ma riportano gli step in forma di lista
prova = scanl (+) 0 [3, 5, 2, 1] -- somma: [0, 3, 8, 10, 11]
-- scanr invece mette il risultato finale come primo elemento

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- FUNZIONE $ O FUNCTION APPLICATION
-- Definizione: ($) :: (a -> b) -> a -> b
--              f $ x = f x
-- Praticamente sembra un operatore inutile, ma nella pratica è un operatore destra-associativo con la più bassa precedenza
-- Solitamente è una comodità per usare meno parentesi
-- sum (map sqrt [1..130])
-- sum $ map sqrt [1..130]

-- COMPOSIZIONE DI FUNZIONI
-- In Haskell si usa il (.) come si usa il puntino in matematica
prova2 :: [Int]
prova2 = map (negate . abs) [5, -3, -6, 7, 3, 2, -19, 24] -- prima applica abs poi negate

-- Se voglio concatenare tante funzioni ed evitare inutili parentesi
fn =
  replicate 100
    . product
    . map (* 3)
    . zipWith
      max
      [1, 2, 3, 4, 5]
    $ [4, 5, 6, 7, 8]