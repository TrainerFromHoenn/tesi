-- Voglio creare un tipo che descriva una persona
data Person = Person String String Int Float String String deriving (Show)

-- Nome, Cognome, età, altezza, numero di telefono, gusto di gelato

-- voglio creare una funzione che separi le info della persona
firstName :: Person -> String
firstName (Person firstName _ _ _ _ _) = firstName

lastName :: Person -> String
lastName (Person _ lastName _ _ _ _) = lastName

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

-- WOW CHE MERDA
-- metodo più facile -> persona.hs