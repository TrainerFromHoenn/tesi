-- ed ecco qua la funzione più famosa della storia
-- main = putStrLn "Hello, world!"

main = do
  -- do inizia un set di comandi di un linguaggio imperativo
  putStrLn "Hello, what's your name?"
  name <- getLine -- getLine :: IO String
  putStrLn ("Hey " ++ name ++ ", you rock!")

-- Ogni azione di IO può essere incapsulata in una variabile: tipo foo <- putStrLn "Hello, what's your name"
-- (Però il valore di foo sarebbe = () )
