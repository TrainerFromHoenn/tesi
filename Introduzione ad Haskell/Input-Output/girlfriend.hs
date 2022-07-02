import System.IO

main = do
  handle <- openFile "girlfriend.txt" ReadMode -- openFile prende un path e crea un'azione di IO che apre un file e lo associa a un handle
  contents <- hGetContents handle -- hGetContents prende un handle, estrae il contenuto e lo incapsula
  putStr contents -- Stampa contenuto del file
  hClose handle -- chiude il file stream

-- altro modo
-- main = do
-- withFile "girlfriend.txt" ReadMode (\handle -> do
-- contents <- hGetContents handle
-- putStr contents)

-- withFile include apertura e chiusura file in una linea sola