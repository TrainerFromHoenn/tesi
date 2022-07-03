import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

-- la funzione randoms invece genera una lista infinita di valori
someCoins :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
someCoins 0 gen = ([], gen) -- restituisce una lista vuota e il generatore usato
someCoins n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = someCoins (n -1) newGen -- definizione ricorsiva
   in (value : restOfList, finalGen)

-- random in un range -> randomR / randomRs

-- getStdGen è un'azione I/O, che chiede al sistema un rng