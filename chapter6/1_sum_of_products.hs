{-# LANGUAGE GADTs #-}

-- ------------------------------------------------------

data List' a = Nil' | Cons' a (List' a)
  deriving Show

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
  deriving Show

aList = (Cons' 2 (Cons' 3 (Cons' 5 Nil')))

intTree 
  = Node 2 (Leaf 3) 
           (Node 5 (Leaf 7) 
                   (Leaf 11))

sizeT :: Tree a -> Int
sizeT (Leaf _) = 1
sizeT (Node _ lt rt) = 1 + (sizeT lt) + (sizeT rt)

sizeL :: List' a -> Int
sizeL Nil' = 0
sizeL (Cons' _ xs) = 1 + (sizeL xs)

-- ------------------------------------------------------
-- The sum of products type representation

-- "unit"
data U = U
  deriving Show

-- type "sum"
data Choice a b = L a | R b
  deriving Show

-- type "product"
data Combo a b = Combo a b
  deriving Show

-- e.g. we can represent Cons' as a Combo of 'a' and (List' a)
--  Cons' a (List' a) -> Combo a (List' a)

-- The List and Tree types can be represented as...

type RList a = Choice U (Combo a (List' a))

type RTree a = Choice (Combo U a)
                      (Combo a (Combo (Tree a) (Tree a)))

-- ------------------------------------------------------
-- Translating between the type and representation

fromL :: List' a -> RList a
fromL Nil'          = L U
fromL (Cons' x xs)  = R (Combo x xs)

toL :: RList a -> List' a
toL (L U)             = Nil'
toL (R (Combo x xs))  = (Cons' x xs)

-- TODO Tree to/from

-- Capture the translation functions in one type
-- (an "embedded projection pair"):
data EP d r = EP {from :: (d -> r), 
                  to :: (r -> d)}

main1 = do
       print $ fromL aList
       -- Cons' 2 (Cons' 3 (Cons' 5 Nil'))

       print $ (toL . fromL) aList
       -- R (Combo 2 (Cons' 3 (Cons' 5 Nil')))

-- ------------------------------------------------------
-- Writing a datatype-generic function

{- Type Representation

  data U = U
  data Choice a b = L a | R b 
  data Combo a b = Combo a b
-}

-- GADT to unify type rep
data TypeRep t where 
  RUnit   ::                           TypeRep U
  RChoice :: TypeRep a -> TypeRep b -> TypeRep (Choice a b)
  RCombo  :: TypeRep a -> TypeRep b -> TypeRep (Combo a b)

  RInt    ::                           TypeRep Int
  RType   :: EP d r    -> TypeRep r -> TypeRep d

gSize :: TypeRep a -> a -> Int
gSize RInt              _           = 1
gSize RUnit             U           = 0
gSize (RChoice trA trB) (L a)       = gSize trA a
gSize (RChoice trA trB) (R b)       = gSize trB b
gSize (RCombo  trA trB) (Combo a b) = (gSize trA a) + (gSize trB b)
gSize (RType ep tr)     t           = gSize tr (from ep t)

-- type RList a = Choice U (Combo a (List' a))

rList :: TypeRep a -> TypeRep (List' a)
rList tr = RType (EP fromL toL)
                 (RChoice RUnit (RCombo tr (rList tr)))

-- aList = (Cons' 2 (Cons' 3 (Cons' 5 Nil')))
main2 = do
  print $ gSize (rList RInt) aList
  -- 3

{- evaluates as...

  gSize (rList RInt) aList

  gSize (RType ep listRep) aList 

  gSize listRep (from ep aList)
  
  -- substitute listRep, apply 'from' to aList gSize (RChoice RUnit (RCombo RInt (rList RInt)))
  R (Combo 2 (Cons' 3 (Cons' 5 Nil')))
  
  -- choose the 2nd type-rep because of R in list rep
  gSize (RCombo RInt (rList RInt))
        (Combo  2    (Cons' 3 (Cons' 5 Nil'))

  –- add the matching type-rep and list-rep pairs
  (gSize RInt 2) + (gSize (rList RInt) (Cons' 3 (Cons' 5 Nil'))
  
  –- evalulate (gSize RInt 2)
  1  + (gSize (rList RInt) (Cons' 3 (Cons' 5 Nil'))
  ...
-}

-- Note how the TypeRep constructors guide the pattern matching...

-- ------------------------------------------------------
-- Adding a new datatype

fromT :: Tree a -> RTree a
fromT (Leaf x)       = L (Combo U x)
fromT (Node x lt rt) = R (Combo x (Combo lt rt))

toT :: RTree a -> Tree a
toT (L (Combo U x))             = Leaf x
toT (R (Combo x (Combo lt rt))) = (Node x lt rt)

rTree :: TypeRep a -> TypeRep (Tree a)
rTree tr = RType (EP fromT toT)
                 (RChoice (RCombo RUnit tr) 
                          (RCombo tr (RCombo (rTree tr) (rTree tr))))

-- Now we use the data-type generic gSize on Tree:

{- 
  intTree 
    = Node 2 (Leaf 3) 
             (Node 5 (Leaf 7) 
                     (Leaf 11))
-}
main3 = do
  print $ gSize (rTree RInt) intTree
  -- 5

-- ------------------------------------------------------
