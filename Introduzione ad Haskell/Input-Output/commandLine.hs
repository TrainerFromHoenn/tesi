import Data.List
import System.Environment

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn progName

--runhaskell .\commandLine.hs first second third "multi word arg"
--The arguments are:
--first
--second
--third
--multi word arg
--The program name is:
--commandLine.hs