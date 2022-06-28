-- definizione di maximum recorsivamente
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "lista vuota"
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs -- pattern matching e ricorsione vanno benissimo insieme

-- replicate prende un Int e un altro elemento e restituisce una lista che ha x ripetizioni dello stesso elemento
-- replicate 3 5 -> [5, 5, 5]
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n -1) x -- mette x davanti e richiama replicate con n-1

-- forma alternativa di take
-- take 3 [4,5,6,7,8,9] -> [4,5,6]
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n -1) xs

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- zip (incolla 2 liste)
-- zip [1,2,3] [1,2] = [(1,1) (2,2)]
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- QUICKSORT!

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted
