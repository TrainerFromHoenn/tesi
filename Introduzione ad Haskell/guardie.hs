-- Molto simili agli if statements

-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
--   | weight / height ^ 2 <= 18.5 = "Madonna sei secchissimo uomo!"
--   | weight / height ^ 2 <= 25.0 = "Sei normale, normie di merda"
--   | weight / height ^ 2 <= 30.0 = "Ciccione, va in palestra"
--   | otherwise = "DAMN!" -- otherwise è definito come otherwise = True

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Madonna sei secchissimo uomo!"
  | bmi <= normal = "Sei normale, normie di merda"
  | bmi <= fat = "Ciccione, va in palestra"
  | otherwise = "DAMN!"
  where
    bmi = weight / height ^ 2 -- where serve a definire var locali
    -- skinny = 18.5
    -- normal = 25.0
    -- fat = 30.0
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- si possono chiamare funzioni in "where"
calcBmis :: (RealFloat a) => [(a, a)] -> [a] -- usa una lista di altezze e pesi e restituisce lista bmi
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

-- let a differenza di where è molto più locale
-- let <bindings> in <expression>

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea -- i let sidearea e toparea vengono usati qui

-- puoi infilare let in mezzo a qualsiasi expr

-- i case expr si fanno col pattern matching
-- infatti questo codice
-- head' :: [a] -> a
-- head' [] = error "Lista vuota!"
-- head' (x : _) = x
-- equivale a

head' :: [a] -> a
head' xs = case xs of
  [] -> error "Lista vuota!"
  (x : _) -> x

-- I case sono utili per fare pattern matching in mezzo a un'expr
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
  where
    what [] = "empty."
    what [x] = "a singleton."
    what xs = "a longer list."