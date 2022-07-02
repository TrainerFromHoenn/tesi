-- comando getContents
-- azione IO che legge il contenuto dello standard input fino al carattere EOF
-- utile per fare pipeline da output programma a input di un altro

import Control.Monad
import Data.Char

main = do
  contents <- getContents
  putStr (map toUpper contents)