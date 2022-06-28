-- lambda = funzione anonima passata a funzioni high order
-- per fare una lambda si usa \ più i parametri, poi -> e il corpo funzione

addThree :: (Num a) => a -> a -> a -> a
addThree = \x y z -> x + y + z
