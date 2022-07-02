-- main = do
--  contents <- getContents
--  putStr (shortLinesOnly contents)

-- questo pattern di input infiniti è talmente frequente che esiste la funzione INTERACT
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input -- lines crea un array usando come separatore \n
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
   in result

-- potrei addirittura stringere facendo
-- main = interact $ unlines . filter ((<10) . length ) . lines