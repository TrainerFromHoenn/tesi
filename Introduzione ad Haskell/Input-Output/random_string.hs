import Data.List
import System.Random

-- Programma che genera una stringa casuale
main = do
  gen <- getStdGen -- getStdGen utilizza il sistema per creare un seed casuale
  let randomChars = randomRs ('a', 'z') gen
      (first20, rest) = splitAt 20 randomChars
      (second20, _) = splitAt 20 rest
  putStrLn first20
  putStr second20

-- N.B. se chiami getStdGen 2 volte, viene usato comunque lo stesso seed
-- un modo per fare doppia stringa è farne una lunga il doppio e splittarla in due

-- un altro modo è usare "newStdGen", che crea un nuovo random E RESETTA quello iniziale, così possiamo richiamare getStdGen