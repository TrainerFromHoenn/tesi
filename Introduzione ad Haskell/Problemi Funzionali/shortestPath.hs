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
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem -- si fa un foldl su tutto il sistema stradale
   in if sum (map snd bestAPath) <= sum (map snd bestBPath) -- guardiamo quali dei due è meno costoso
        then reverse bestAPath -- lo restituiamo inverso
        else reverse bestBPath

-- Funzione che scorre da sinistra a destra in una lista? FOLDL

-- La prima cosa che si fa di solito è prendere una coppia di Path, guardare una nuova sezione
-- e decidere la nuova coppia
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA -- prima di tutto calcolo il prezzo di A e B totale finora
      priceB = sum $ map snd pathB -- (se il pathA è (A,100), (C,20) otterrò 120
      forwardPriceToA = priceA + a -- costo se andassi ad A dritto da A
      crossPriceToA = priceB + b + c -- costo se andassi ad A facendo un cross da B
      forwardPriceToB = priceB + b -- stessa cosa per b
      crossPriceToB = priceA + a + c
      newPathToA =
        if forwardPriceToA <= crossPriceToA -- se vince il forward faccio solo l'append del tratto A
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB -- altrimenti faccio il cross
      newPathToB =
        if forwardPriceToB <= crossPriceToB
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
   in (newPathToA, newPathToB)

-- Ora invece creiamo un modo per renderlo leggibile meglio il risultato
groupsOf :: Int -> [a] -> [[a]] -- prende una lista e la splitta in due gruppi dello stesso size
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs) -- raggruppa le prime n, poi ripete sul resto della lista
-- es. [1..10] -> [1,2,3] : grousOf [4,5,6,7,8,9,10]

-- Finalmente si passa al main
main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents) -- in questo modo posso creare una lista come heatrowLondon dall'input
      roadSystem = map (\[a, b, c] -> Section a b c) threes -- Qua creo la struttura che serve al codice
      path = optimalPath roadSystem
      pathString = concatMap (show . fst) path -- concatMap è concat $ map
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice