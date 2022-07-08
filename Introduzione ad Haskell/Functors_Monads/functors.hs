-- Riepilogo: I functors son roba che si può mappare su liste, alberi, Maybe s, e roba varia
-- hanno un unico metodo typeclass: fmap
-- fmap :: (a -> b) -> f a -> f b
-- "dammi una funzione che prende a e restituisce b
-- e un box con a dentro, e ti restituisco una scatola di b"

-- Vediamo come IO è istanza di Functor
-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

-- Voglio riscrivere questo codice con i Functors
-- main = do
--   line <- getLine
--   let line' = reverse line
--   putStrLn $ "You said " ++ line' ++ " backwards!"
--   putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

-- sempicemente:
main = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
