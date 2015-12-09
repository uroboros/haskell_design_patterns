f :: Num a => a -> a
f x = (^2) x

main1 = print (f 42)
        -- 1764

-- ------------------------------------------------------

data Maybe' a = Just' a | Nothing'
  deriving (Show)

-- To achieve 
-- f (Just' 42)
-- we could either write a new function
fMaybe _ Nothing' = Nothing'
fMaybe f (Just' x) = Just' (f x)

main2 = print $ fMaybe f (Just' 42)
        -- Just' 1764

-- ------------------------------------------------------

-- or, preferably, make Maybe' an instance of Functor
instance Functor Maybe' where
  fmap _ Nothing' =   Nothing'
  fmap f (Just' x) =  Just' (f x)

{- 
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

main3 = do
  print $ fmap f (Just' 42)
  -- Just' 1764

  print $ fmap show (Just' 42)
  -- Just' "42"

-- ------------------------------------------------------

-- Functor Laws

{-
   -- law of composition
  fmap (f . g)  ==  fmap f . fmap g
  -- e.g.
  fmap (f . read) getLine  
    = ((fmap f) . (fmap read)) getLine

  -- identity law
  fmap id  ==  id
  -- e.g.
  fmap id (Just 1) = id (Just 1)
-}

-- ------------------------------------------------------

main4 = do
  print $ fmap (^2) [1, 2, 3, 5, 7]
  -- same as List.map
  print $ map (^2) [1, 2, 3, 5, 7]
  -- [1,4,9,25,49]


