
-- PATTERN MATCHING
-- nel definire funzioni posso creare diverse versioni della stessa funzione per diversi pattern di input
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- con il pattern matching possiamo toglierci un sacco di if else complessi
sayMe :: (Integral a) => a -> String
sayMe 1 = "UNO!"
sayMe 2 = "DUE!"
sayMe 3 = "TRE!"
sayMe 4 = "QUATTRO!"
sayMe 5 = "CINQUE!"
sayMe x = "FUORI!"   -- Importante che sia in fondo, altrimenti qualsiasi numero matcherebbe con x!

-- FINALMENTE RICORSIONE!
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- con le tuple
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b) -- funziona ma modo migliore:
addVectors (x1, x2) (y1, y2) = (x1 + x2, y1 + y2) -- si sfrutta il pattern per già sciogliere gli elementi

-- sfrutto per creare first second third per le triple
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

-- Mia versione di head
head' :: [a] -> a
head' [] = error "AO non puoi!"
head' [x] = x
head' (x:_) = x

-- Funzione che ci dice semplicemente che elementi ci sono nella lista
tell :: (Show a) => [a] -> String
tell [] = "Lista vuota, dai mettice qualcosa"
tell [x] = "C'è solo un: " ++ show x ++ " solo soletto porello"
tell [x, y] = "Bravo mo ci sono due robe: " ++ show x ++ " e " ++ show y
tell (x:y:_) = "Ebbasta ao c'hai messo troppa robba, te dico i primi due: " ++ show x ++ " e " ++ show y

-- Length con ricorsione
length' :: (Num b) => [a] -> b 
length' [] = 0                         -- Lunghezza lista vuota: 0
length' (_:xs) = 1 + length' xs        -- Lunghezza lista generica = 1 + lunghezza lista senza il primo elemento

sum' :: (Num a) => [a] -> a 
sum' [] = 0
sum' (x:xs) = x + sum' xs


