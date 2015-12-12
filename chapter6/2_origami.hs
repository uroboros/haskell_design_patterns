-- ------------------------------------------------------
-- Tying the recursive knot

{- from earlier...

  data List' a = Nil' | Cons' a (List' a)
  data Tree a = Leaf a | Node a (Tree a) (Tree a) 
-}

data Fix s a = FixT {getFix :: s a (Fix s a)}

-- rephrase List_ and Tree_ so that we can express them using the Fix type
-- ('r' represents the recursion of the type)
data List_ a r = Nil_    | Cons_ a r
  deriving Show

data Tree_ a r = Leaf_ a | Node_ a r r
  deriving Show

-- List_ and Tree_ don't explicitly recur, so that Fix
-- "ties the recursive knot around the shape"

-- Now, we can write
type ListF a = Fix List_ a
type TreeF a = Fix Tree_ a

-- We can construct List_ lists in the familiar way:
aList1 = Cons_ 12 Nil_
-- aList :: List_ Integer (List_ a r)
aList2 = Cons_ 12 (Cons_ 13 Nil_)
-- aList2 :: List_ Integer (List_ Integer (List_ a r))

-- To construct ListF lists:

aListF1 = (Cons_ 13 (FixT Nil_))
-- aListF1 :: List_ Integer (Fix List_ a)

aListF2 = (FixT (Cons_ 13 (FixT Nil_)))
-- aListF2 :: Fix List_ Integer

aListF3 = (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))
-- aListF3 :: List_ Integer (Fix List_ Integer)

aListF :: ListF Integer -- type alias for "Fix List_ Integer"
aListF = FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))

-- ------------------------------------------------------
-- Generic Map

-- mapL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mapL f listF = case list_ of
  (Cons_ x r) -> FixT $ Cons_ (f x) (mapL f r)
  Nil_        -> FixT Nil_
  where
    list_ = getFix listF

-- This is clumsy because we have to unwrap the list (with getFix) 
-- and then re-wrap the result with FixT in both base clauses.

showListF :: (Show a) => ListF a -> String
showListF (FixT (Cons_ x r)) 
  = (show x) ++ ", " ++ (showListF r)
showListF (FixT (Nil_))      
  = "Nil_"

main1 = putStrLn . showListF $ mapL (*2) aListF
        -- 24, 26, Nil_

-- gmap with BIFUNCTOR >>>>>>>>>>>>>>>>>>>>>>>

-- Bifunctor is just a Functor that applies 2 functions 
-- to a type instead of one:
class Bifunctor s where
  bimap :: (a -> c) -> (b -> d) -> (s a b -> s c d)
-- (already defined in Data.Bifunctor)

instance Bifunctor List_ where
  bimap f g Nil_        = Nil_
  bimap f g (Cons_ x r) = Cons_ (f x) (g r)

instance Bifunctor Tree_ where
  bimap f g (Leaf_ x)       = Leaf_ (f x)
  bimap f g (Node_ x rl rr) = Node_ (f x) (g rl) (g rr)

-- note: 'g' is applied to the recursive part(s) of the data type

-- Now that the List_ and Tree_ types are bifunctors, 
-- we can write a generic map:
gmap :: Bifunctor s => (a -> b) -> Fix s a -> Fix s b
gmap f = FixT . bimap f (gmap f) . getFix
-- * the 1st function passed to bimap is the mapping function 'f'
-- * the 2nd function is (gmap f), which explicitly applies gmap 
--   to the recursive part of the data structure.

main2 = putStrLn . showListF $ gmap (*2) aListF
        -- 24, 26, Nil_


-- ------------------------------------------------------
-- Generic Fold

-- generic fold with bimap
gfold :: Bifunctor s => (s a b -> b) -> Fix s a -> b
gfold f = f . bimap id (gfold f) . getFix

-- gfold replaces occurrences of FixT with f:

-- FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))
-- f    (Cons_ 12 (f    (Cons_ 13 (f    Nil_))))

-- To fold together a sum, we create an adder:
addL (Cons_ x r) = x + r
addL Nil_        = 0

-- aListF = FixT (Cons_ 12 (FixT (Cons_ 13 (FixT Nil_))))
main3 = print $ gfold addL aListF
        -- 25


-- ------------------------------------------------------
-- Generic Unfold

-- fold consumes data structures, unfold produces them by 
-- unfolding a structure from a single value:

-- unfold a List
unfoldL stopF nextF val
  = if stopF val -- stop if True
      then []
      else val : (unfoldL stopF nextF (nextF val))

main4 = print $ unfoldL (< (-10)) (\x -> x - 1) 10
        -- [10,9,8,7,6,5,4,3,2,1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]

-- We can use bimap to create a generic unfold:
gunfold :: Bifunctor s => (b -> s a b) -> b -> Fix s a
gunfold f = FixT . bimap id (gunfold f) . f

-- for example
toList 0 = Nil_
toList n = (Cons_ n (n-1))

main5 = putStrLn . showListF $ gunfold toList 10
--     10, 9, 8, 7, 6, 5, 4, 3, 2, 1, Nil_

{- evaluates as...
  
  gunfold toList 10
  (FixT . bimap id (gunfold toList) . toList) 10
  (FixT . bimap id (gunfold toList)) (Cons_ 10 9) -- toList 10

  FixT $  bimap id (gunfold toList) (Cons_ 10 9)
  FixT $  Cons_ (id 10) (bimap id (gunfold toList) 9)
  (FixT Cons_ 10 (FixT Cons_ 9 (bimap id (gunfold toList) 8)) 
  ....

-}

-- ------------------------------------------------------
-- Generic unfold + fold: hylomorphism

main6 = print $ gfold addL (gunfold toList 100)
        -- 5050

-- unfold and fold are each other's mirror images:

-- gunfold f = FixT . bimap id (gunfold f) . f
-- gfold f   = f    . bimap id (gfold f)   . getFix

-- hylo is their composition:
hylo f g = g . bimap id (hylo f g) . f

main7 = print $ hylo toList addL 100
        -- 5050
