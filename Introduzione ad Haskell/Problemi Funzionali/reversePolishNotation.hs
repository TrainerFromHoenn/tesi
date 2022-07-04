-- Reverse Polish Notation: notazione per le operazioni matematiche, con notazione suffissa e non infissa
-- es. 10 - (4+3) *2 diventerà 10 4 3 + 2 * -
-- Funziona tramite uno stack, si parte da sinistra e si mette ogni elemento sullo stack
-- Appena si arriva a un'operazione la si compie sugli elementi in cima allo stack
-- Metto 10 (10)
-- Metto 4 (4.10)
-- Metto 3 (3.4.10)
-- Faccio + sulla cima (3+4.10)
-- Soluzione (7.10)
-- Metto 2 (2.7.10)
-- Faccio * (2*7.10)
-- Soluzione (14.10)
-- Faccio - (10 - 14)
-- Soluzione: unico elemento sullo Stack-> -4

import Data.List

-- Parto dalla dichiarazione
solveRPN :: String -> Float
-- dividerò la stringa di numeri in elementi singoli, usando lo spazio come divisore
-- usiamo un'azione da sx a destra creando uno stack -> FOLDL
-- come stack uso una lista, e la cima sarà la head della lista
solveRPN = head . foldl foldingFunction [] . words -- [] è l'accumulatore, in questo caso uno stack vuoto -- words crea l'array a partire dagli spazi vuoti
-- head è il risultato finale
  where
    foldingFunction (x : y : ys) "*" = (x * y) : ys
    foldingFunction (x : y : ys) "+" = (x + y) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction (x : y : ys) "/" = (y / x) : ys
    foldingFunction (x : y : ys) "^" = (y ** x) : ys
    foldingFunction (x : xs) "ln" = log x : xs
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numberString = read numberString : xs

-- Comodissima implementazione perché è scalabile quando vogliamo (posso aggiungere un operatore nuovo alla lista)
-- Aggiunti / ^ ln sum
