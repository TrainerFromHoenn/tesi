
-- ":t expr" -> valuta il tipo
removeNonUppercase :: [Char] -> [Char] -- è buona pratica mettere i tipi della funzione nella definizione
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- si usa una lettera al posto di un tipo quando si vuole creare una funzione polimorfica
-- head :: [a] -> a

-- Le classi di tipo sono invece interfacce che definiscono un comportamento
-- :t (==)
-- (==) :: (Eq a) => a -> a -> Bool
-- tutto ciò che sta prima => sono i class constraint (in questo caso, parametri dello STESSO tipo -> superclasse Eq)
-- un altro esempio è Ord, che contiene i tipi che permettono ordinamento 
-- Show = possono essere rappresentati da stringhe
-- read expr -> trasforma le stringhe in elementi di altri tipi per risolvere l'espressione
esempioRead = read "8.2" + 3.8 -- restituisce 12.0
-- non si può fare solo read "4", ha bisogno di un'altra referenza
-- si può forzare con read "4" :: Int per esempio

-- Enum tipi ordinati sequenzialmente, che hanno definito succ e pred
-- Bounded, tipi con max e min
-- Num: superclasse numerica
-- Integral: sottoclasse di num, contiene Integer e Int
-- Floating: contiene float e double
