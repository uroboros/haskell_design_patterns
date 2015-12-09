{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Applicative

-- ------------------------------------------------------
-- The Traversable typeclass is to mapM what Foldable is to fold:

{-
    -- required: traverse or sequenceA
    class (Functor t, Foldable t) => Traversable (t :: * -> *) where
      -- APPLICATIVE form
      traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
      sequenceA :: Applicative f => t (f a) -> f (t a)

      -- MONADIC form
      mapM      :: Monad m       => (a -> m b) -> t a -> m (t b)
      sequence  :: Monad m       => t (m a) -> m (t a)
-}

-- traverse generalizes our mapA function to all Foldable containers.

-- Similarly, T.mapM is a more general version of "Prelude mapM" for lists:
{- 
      T.mapM :: Monad m => (a -> m b) -> t a -> m (t b)
      mapM   :: Monad m => (a -> m b) -> [a] -> m [b]
-}

-- ------------------------------------------------------
-- A Traversable Tree (the hard way)

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
  deriving Show

-- A Traversable container must also be a Functor and Foldable:
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x lTree rTree) 
    = (Node (f x) 
            (fmap f lTree) 
            (fmap f rTree))

instance Foldable Tree where 
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node x lTree rTree)
    = (foldMap toMonoid lTree) 
      `mappend` (toMonoid x)
      `mappend` (foldMap toMonoid rTree)

instance Traversable Tree where
  -- traverse  :: Applicative ma => (a -> ma b) -> mt a -> ma (mt b)
  traverse g (Leaf x) = Leaf <$> (g x)
  traverse g (Node x ltree rtree) 
    = Node <$> (g x) <*> (traverse g ltree) <*> (traverse g rtree)


tree1 = Node 2 (Leaf 3) 
               (Node 5 (Leaf 7) 
                       (Leaf 11))

doF n = do print n; return (n * 2)

main1 = traverse doF aTree
{- 
  2
  3
  5
  7
  11

  Node' 4 (Leaf' 6) 
          (Node' 10 (Leaf' 14) 
                    (Leaf' 22))
-}

-- ------------------------------------------------------
-- Traversable Tree using
--    {-# LANGUAGE DeriveFunctor #-}
--    {-# LANGUAGE DeriveFoldable #-}
--    {-# LANGUAGE DeriveTraversable #-}

data Tree' a = Node' a (Tree' a) (Tree' a)
            | Leaf' a
  deriving (Show, Functor, Foldable, Traversable)

aTree = Node' 2 (Leaf' 3) 
                (Node' 5 (Leaf' 7) 
                         (Leaf' 11))

main2 = traverse doF aTree
