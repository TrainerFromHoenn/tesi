-- Stessa funzione di exceptions.hs ma sta volta usiamo catch

import Control.Exception (catch)
import System.Environment
import System.IO
import System.IO.Error

main = catch toTry handler

toTry :: IO () -- toTry è l'azione IO che voglio compiere
toTry = do
  (fileName : _) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO () -- handler è il gestore eccezioni
handler e
  | isDoesNotExistError e = putStrLn "Whoops, had some trouble!"
  | otherwise = ioError e -- Per tutti gli errori che non sono "il file non esiste"