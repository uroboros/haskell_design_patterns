{-# LANGUAGE Rank2Types #-}
-- ------------------------------------------------------

tupleF elemF (x, y) = (elemF x, elemF y)

-- GHC infers type 
--   tupleF :: (a -> b) -> (a, a) -> (b, b)

main1 = do
  -- we can do this...
  
  print $ tupleF length ([1,2,3], [3,2,1])
  -- (3,3)
  print $ tupleF show   (1, 2)
  -- ("1","2")
  print $ tupleF show   (True, False)
  -- ("True","False")

  -- but not this...
  -- tupleF show   (True, 2)
  -- tupleF show ([True, False], [1, 2])

-- ------------------------------------------------------

{- Rank2 Type:

  tupleF' :: (forall a . a -> b) -- elemF
              -> (a1, a2)        -- input tuple
              -> (b, b)          -- output tuple
-}

-- (we constrain the a, a1, a2 type variables to be Showable 
--  so that we can print the tuples)
tupleF' :: (Show a1, Show a2) =>
          (forall a . Show a => a -> b) -- elemF
            -> (a1, a2)                 -- input tuple
            -> (b, b)                   -- output tuple
tupleF' elemF (x, y) = (elemF x, elemF y)

main2 = do
    -- same as before...
    print $ tupleF' show (1, 2)
    print $ tupleF' show (True, False)
    -- and now we can do this... 
    print $ tupleF' show (True, 2)
    -- ("True","2")
    print $ tupleF' show ([True, False], [1, 2])
    -- ("[True,False]","[1,2]")
