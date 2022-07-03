-- uso gli args da command line per unire appendTodo e deleteTodo

import Control.Exception (handle)
import Data.List
import System.Directory
import System.Environment
import System.IO

-- inizio creando una lista associativa con le chiavi dei diversi command line args
dispatch :: [(String, [String] -> IO ())]
dispatch =
  [ ("add", add),
    ("view", view),
    ("remove", remove)
  ]

main = do
  (command : args) <- getArgs -- Se chiamo il programma "todo add todo.txt "Spank the monkey" " command sarà add e gli args tutto il resto
  let (Just action) = lookup command dispatch -- si usa Just nel caso il lookup dia "Nothing"
  action args -- action -> Esegue l'azione di args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
