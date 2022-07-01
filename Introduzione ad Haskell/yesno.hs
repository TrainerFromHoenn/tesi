data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

data TrafficLight = Red | Yellow | Green

class YesNo a where
  yesno :: a -> Bool

-- ora definisco delle istanze
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id -- (id è una funzione della standard lib che prende un parametro e lo restituisce uguale)

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

-- ora metto qualche funzione
yesnoIf :: (YesNo y) => y -> a -> a -> a -- Ricreato l'if
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult