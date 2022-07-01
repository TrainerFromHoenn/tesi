-- Programma che continua a leggere l'input e restituisce le parole invertite
main = do
  line <- getLine
  if null line -- Condizione di chiusura -> linea vuota
    then return () --
    else do
      putStrLn $ reverseWords line
      main -- toh guarda, ricorsione!

reverseWords :: String -> String
reverseWords = unwords . map reverse . words -- words separa la frase in un array di parole, unwords rifà l'array

-- N.B. In Haskell "return" crea un azione IO da un valore puro.
-- return () crea un'azione nulla