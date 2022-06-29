import Data.List

-- Modulo per la gestione di liste
-- INTERSPERSE: prende un elemento e lo mette fra ogni elemento della lista
punto = intersperse '.'

-- INTERCALATE prende una lista e una lista di liste, inserisce la prima fra le altre e "appiattisce"
-- frase = intercalate " "

-- TRANSPOSE crea la trasposta della matrice
matTrasp = transpose

-- esempio, somma di polinomi
sumPolinom = map sum $ transpose [[0, 3, 5, 9], [10, 0, 0, 9], [8, 5, 1, -1]] -- Somma 3x^2+5x+9, 10x^3+9 e 8x^3+5x^2+x-1

-- La libreria contiene anche foldl' e foldl1' che sono la versione "non pigra" dei foldl (per evitare stack overflow)

-- CONCAT appiattice una lista di liste in una lista di elementi
piatto = concat ["foo", "bar", "cat"] -- "foobarcat"

-- CONCATMAP mappa una funzione su una lista e poi la concatena
replicaLista = concatMap (replicate 4) [1 .. 3] -- [1,1,1,1,2,2,2,2,3,3,3,3]

-- AND prende una lista di valori booleani e restituisce True solo se tutti i valori sono True
-- Ovviamente esiste anche OR ANY ALL
verificaMaggiore = all (> 4) [5, 6, 7, 8] -- True (son tutti maggiori di 4)

-- ITERATE prende una funzione e un valore di partenza, e applica all'infinito in catena la funzione ai risultati
grasseRisate = take 10 $ iterate (++ "ah") "ah"

-- SPLITAT prende un numero e una lista, e poi splitta la lista dopo x elementi in due
tagliaNome = splitAt 7 "MicheleBiena"

-- TAKEWHILE prende gli elementi in una lista finché il risultato è True
primaParola = takeWhile (/= ' ') "Questa è una frase" -- "Questa"

-- DROPWHILE fa il contrario, droppa i primi elementi veri finché non ne trova uno falso

-- SPAN è come takewhile ma restituisce 2 liste, ovvero anche la parte droppata (break il corrispondente)

-- SORT mette in ordine
-- GROUP raggruppa elementi di una lista in una sottolista di elementi uguali se adiacenti
raggruppaLettere = group [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 7, 8, 5, 5, 5, 5]

-- sort + group è un'ottima combo

-- INITS e TAILS sono come init e tail ma si applicano ricorsivamente
inizialiRicorsive = inits "MicheleBiena"

-- ISINFIXOF cerca una sottolista all'interno di una lista target
-- ISPREFIXOF/SUFFIXOF cerca se una sottolista è all'inizio o alla fine di una lista

-- PARTITION divide una lista in 2, elementi che danno True al predicato ed elementi che danno False
dividiMaiuscole = partition (`elem` ['A' .. 'Z']) "MICHELEbienaEMOLTObello"

-- FIND prende una lista e un predicato e restituisce il primo elemento che soddisfa il predicato
-- l'elemento è wrappato nel valore "Maybe" (può assumere Just something o Nothing)

trovaMaggiore = find (> 4) [1, 2, 3, 4, 5, 6]

-- ELEMINDEX è come elem ma non restituisce il valore booleano, ma la posizione dell'elemento
-- ELEMINDICES trova multiple occorrenze
-- FINDINDEX restituisce l'indice del risultato di FIND
