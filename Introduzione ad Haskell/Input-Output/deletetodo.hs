import Data.List
import System.Directory
import System.IO

main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp" -- Funzione per creare un file temporaneo con il nome iniziante per temp + char random
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks -- zippa una numerazione alla lista di azioni in "contents"
  putStrLn "These are your TO-DO items:"
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks -- !! restituisce un elemento da una lista a quell'indice
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt" -- essenzialmente il file temporaneo diventa il nuovo file, e il vecchio viene eliminato
