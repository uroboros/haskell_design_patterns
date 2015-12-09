import Control.Applicative

-- ------------------------------------------------------
-- ... map over lists with Monadic functions:

doF n = do print n; return (n * 2)
main1 = do
  print $ map (* 2) [2, 3, 5, 7]
  -- [4,6,10,14]

  mapM  doF [2, 3, 5, 7] >>= print  -- display result
  {- 
    2
    3
    5
    7
    [4,6,10,14]
  -}

  mapM_ doF [2, 3, 5, 7]            -- mapM_ discards result
  {- 
    2
    3
    5
    7
  -}

-- ------------------------------------------------------

{- 
  map   :: (a -> b)   -> [a] -> [b]
  mapM  :: (a -> m b) -> [a] -> m [b]
  mapM_ :: (a -> m b) -> [a] -> m ()
-}

-- ------------------------------------------------------
-- map function that maps over Applicatives:

mapA :: Applicative f => (a -> f t) -> [a] -> f [t]
mapA f = sequenceA' . (map f)

sequenceA' :: Applicative f => [f t] -> f [t]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> (sequenceA' xs)

-- import Control.Applicative
main2 = mapA doF [2, 3, 5] >>= print
        {- 
          2
          3
          5
          [4,6,10]
        -}

{- ... evaluating from left to right...

     sequenceA' . (map doF) [2, 3, 5]
     sequenceA'   ((doF 2) : ((map doF) [3, 5]))
     (:) <$> (doF 2) <*> sequenceA' ((map doF) [3, 5])
     (4:)            <*> sequenceA' ((map doF) [3, 5])
     (4:)            <*> sequenceA' ((doF 3) : ((map doF) [5])
     (4:)            <*> (:) <$> (doF 3) <*> sequenceA' [doF 5]
     (4:)            <*> (6:)            <*> sequenceA' [doF 5]
     (4:)            <*> (6:)            <*> (:) <$> (doF 5) <*> sequenceA' []
     (4:)            <*> (6:)            <*> (10:) <*> sequenceA' []
     (4:)            <*> (6:)            <*> (10:) <*> pure []
     4:6:10:[]
     [4, 6, 10]
-}

-- ------------------------------------------------------

-- mapA does DOUBLE traversal (once for map, once for sequenceA).
-- However, if we define mapA in the following way, we have only single traversal:

mapA' f [] = pure []
mapA' f (x:xs) = (:) <$> f x <*> (mapA' f xs)

main3 = mapA' doF [2, 3, 5] >>= print

-- Given mapA, we can define sequenceA in terms of it
-- 	sequenceA = mapA id

-- i.e. mapA and sequenceA can be defined interdependently:
-- 	 mapA f = sequenceA . (map f)
-- 	 sequenceA = mapA id
