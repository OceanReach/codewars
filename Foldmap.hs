module Foldmap where
import Data.Foldable (foldMap, Foldable)
import Data.Monoid

data NiMa a = Mi a | Niz

instance Ord a => Monoid (NiMa a) where
  mempty = Niz

  Niz `mappend` nmx@(Mi _) = nmx
  nmx@(Mi _) `mappend` Niz = nmx

  (Mi x1) `mappend` (Mi x2)
    | x1 > x2 = Mi x2
    | x1 < x2 = Mi x1
    | otherwise = Niz
  
{-
instance Ord a => Semigroup (NiMa a) where
  Niz <> nmx@(Mi _) = nmx
  nmx@(Mi _) <> Niz = nmx

  (Mi x1) <> (Mi x2)
    | x1 > x2 = Mi x2
    | x1 < x2 = Mi x1
    | otherwise = Niz

-}

--turn any foldable into list
myToList :: Foldable t => t a -> [a]
myToList = (\f -> f []) . foldMap (\x -> (x:))


niMa2Maybe :: NiMa a -> Maybe a
niMa2Maybe (Mi a) = Just a
niMa2Maybe Niz = Nothing


--fuck, let's find the minimum
myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = niMa2Maybe . foldMap (\x -> Mi x)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--just a try: myFoldr f ori = foldMap (\x -> f x ori)
myFoldr f ori t = appEndo (foldMap (Endo . f) t) ori
