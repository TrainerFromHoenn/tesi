-- ghci per attaccare Haskell
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
-- Raddoppia se e solo se x è minore di 100
doubleSmallNumber x = if x > 100
    then x
    else doubleMe x

-- Set Comprehension
doubleOneToTen = [x*2 | x <- [1..10]]

overTwelve = [x*2 | x <- [1..10], x*2 >= 12 ]

boomBang xs = [if even x then "BOOM!" else "BANG!" | x <- xs ]

-- Set Comprehension con liste multiple
xy = [x*y | x <- [2, 5, 10], y <- [8, 10, 11]] -- Restituisce TUTTE le possibili combinazioni

combina = [agg ++ " " ++ nom | agg <- ["stupidu", "bellu", "bruttu", "carinu", "simpaticu", "intelligentu"], nom <- ["lu", "michele", "babi", "kora"]]

length' lista = sum [1 | _ <- lista]

removeUpperCase parola = [c | c <- parola, c `elem` ['a'..'z']]

-- Tuple
-- Tupla scorretta: [(1, 2), (8, 11, 5), (4, 5)] -> (8, 11, 5) non corrisponde alla struttura
triangles = [(a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10]]
rightTriangles = [(a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b ^ 2 == c^2]