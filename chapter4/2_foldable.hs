import Data.Monoid
import qualified Data.Foldable as F
import qualified Control.Monad as M

-- ------------------------------------------------------

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
  deriving Show

-- Now we can write a Monoidal fold, which will accumulate tree elements
-- using the underlying type's `mappend`.
foldT :: Monoid a => Tree a -> a
foldT (Leaf x) = x
foldT (Node x lTree rTree)
  = (foldT lTree) `mappend` x `mappend` (foldT rTree)

main1 = do 
  print . foldT $ Node (Sum 2) (Leaf (Sum 3)) 
                               (Leaf (Sum 5))
  -- Sum {getSum = 10}
  print . foldT $ Node (Product 2) (Leaf (Product 3)) 
                                   (Leaf (Product 5))
  -- Product {getProduct = 30}

-- ------------------------------------------------------

-- simplify with toMonoid
foldT' :: Monoid a => (t -> a) -> Tree t -> a
foldT' toMonoid (Leaf x) = toMonoid x
foldT' toMonoid (Node x lTree rTree)
  = (foldT' toMonoid lTree) 
    `mappend` (toMonoid x)
    `mappend` (foldT' toMonoid rTree)

aTree = Node 2 (Leaf 3) 
               (Node 5 (Leaf 7) 
                       (Leaf 11))


main2 = do 
  print $ foldT' Sum           aTree
  -- Sum {getSum = 10}
  print $ foldT' Product       aTree
  -- Product {getProduct = 30}
  print $ foldT' (Any . (==5)) aTree
  -- Any {getAny = True}
  print $ foldT' (All . (>0))  aTree
  -- All {getAll = True}
  where 
    aTree = Node 2 (Leaf 3) (Leaf 5)

-- ------------------------------------------------------
-- FOLDABLE

{-
  class Foldable (t :: * -> *) where
    -- implement foldMap or foldr
    foldMap :: Monoid m => (a -> m)           -> t a -> m
    foldr   ::             (a -> b -> b) -> b -> t a -> b  
    -- get these for free:
    -- fold, foldl, fold, foldr', foldl', foldr1, foldl1
-}

-- foldMap generalizes our foldT':
{- 
  foldT'  :: Monoid a => (t -> a) -> Tree t -> a
  foldMap :: Monoid m => (a -> m) -> t a    -> m
  fold    :: Monoid m =>             t m    -> m
-}
-- 'fold' assumes monoids and does not need the toMonoid function:
--  fold = foldmap id

instance F.Foldable Tree where 
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node x lTree rTree)
    = (F.foldMap toMonoid lTree) 
      `mappend` (toMonoid x)
      `mappend` (F.foldMap toMonoid rTree)

-- import Data.Foldable
main3 = do
  print $ F.foldMap Sum           aTree
  -- Sum {getSum = 10}
  print $ F.foldMap Product       aTree
  -- Product {getProduct = 30}
  print $ F.foldMap (Any . (==5)) aTree
  -- Any {getAny = True}
  print $ F.foldMap (All . (>0))  aTree
  -- All {getAll = True}
  where aTree = Node 2 (Leaf 3) (Leaf 5)

-- Instead of just implementing fold for Tree, 
-- we've turned Tree into a Foldable container.

-- Data.Foldable comes with many convenience functions 
-- that generalize the corresponding Prelude functions,

-- ------------------------------------------------------
-- ... using Foldable helper functions

main4 = do
  print $ F.sum aTree
  -- 10
  print $ F.product aTree
  -- 30
  print $ F.any (==5) aTree
  -- True
  print $ F.all (>0) aTree
  -- True
  print $ F.maximum aTree
  -- 5
  where aTree = Node 2 (Leaf 3) (Leaf 5)


{- Foldable generalises from List to foldable type 

  F.sum :: (Foldable t, Num a) => t a -> a
  sum ::                Num a  => [a] -> a 


  F.foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
  foldM    ::              Monad m  => (b -> a -> m b) -> b -> [a] -> m b
-}


doSum = F.foldrM doPlus
  where 
    doPlus acc x = do
      putStrLn $ " + " ++ (show x) ++ " = " ++ (show acc) 
      return (acc + x)

main5 = doSum 0 aTree
{- 
   + 0 = 11
   + 11 = 5
   + 16 = 7
   + 23 = 2
   + 25 = 3
  28
-}

-- ------------------------------------------------------

-- All Foldable things can be expressed as lists by folding (:) over the Foldable, e.g.
main6 = do
  print $ F.toList aTree
  -- same as
  print $ F.foldr (:) [] aTree
  -- [3,2,7,5,11]
