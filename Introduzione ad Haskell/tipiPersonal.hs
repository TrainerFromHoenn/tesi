-- Se voglio esportare tutto come un modulo posso fare
module Shapes
  ( Point (..), -- includo i costruttori di classe
    Shape (..),
    surface,
    nudge,
  )
where

-- Si possono creare i propri tipi di dato con la keyword data

data Bool = False | True

-- False | True sono i costruttori di tipo
-- data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

-- LEGGI LINEA 19

-- Ora rendo migliore il tipo di dato creando il punto
data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Creiamo una forma, può essere cerchio o rettangolo tipo
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- Ora posso creare una funzione che usa questo tipo
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- Posso chiamare la funzione facendo surface $ Circle a b c per esempio

-- Ora creo una funzione che sposta la forma
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))
