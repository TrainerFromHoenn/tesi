-- il typeclass Functor serve per "mappare cose". Le liste ad esempio fanno parte di questa typeclass
-- implementazione:
-- class Functor f where
-- fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
-- fmap = map