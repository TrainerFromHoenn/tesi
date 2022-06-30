data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String, -- I tipi usati possono essere polimorfici (es. "a") ma non sempre è utile farlo
    flavor :: String
  }
  deriving (Eq, Show, Read)

-- La comodità è che sono già state create in automatico funzioni che fanno il lookup dei campi
-- Inoltre cambia anche l'output di "Show" mettendo i nomi dei campi oltre ai valori
