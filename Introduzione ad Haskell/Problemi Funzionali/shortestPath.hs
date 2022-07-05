-- Problema di percorso minimo
-- Grafo con rami pesati -> trova il percorso a costo minimo
-- Voglio un programma che dato un elenco di costi mi crei il percorso minore
-- La struttura è sempre: strada A, strada B e Incrocio
-- L'ultimo elemento sarà 0 per facilitare l'albero

-- Come rappresento la strada?
-- Utilizzo 2 oggetti: il nodo e la strada
-- data Node = Node Road (Maybe Road) -- O è nodo con due strade, una che porta a un altro nodo
-- e una che porta a un'altra strada, oppure un nodo
-- finale
-- data Road = Road Int Node -- Quanto lunga e a che nodo arriva

-- c'è però un modo più semplice di rappresentare la struttura dati?
-- Devo solo guardare una sezione di strade per volta
data Section = Section
  { getA :: Int,
    getB :: Int,
    getC :: Int
  }
  deriving (Show)

type RoadSystem = [Section] -- Il sistema di strade è una lista di sezioni da 3 strade

-- posso rappresentare il nostro sistema ora
heatrowLondon :: RoadSystem
heatrowLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show) -- La strada scelta è la A, B o C

type Path = [(Label, Int)] -- Un percorso è una lista di strade con un nome e un peso

optimalPath :: RoadSystem -> Path -- Ora la dichiarazione è easy
-- Funzione che scorre da sinistra a destra in una lista? FOLDL

-- La prima cosa che si fa di solito è prendere una coppia di Path, guardare una nuova sezione
-- e decidere la nuova coppia
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA =
        if forwardPriceToA <= crossPriceToA
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB
      newPathToB =
        if forwardPriceToB <= crossPriceToB
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
   in (newPathToA, newPathToB)
