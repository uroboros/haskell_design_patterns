{-# LANGUAGE TemplateHaskell #-}
import Data.Monoid
import Control.Lens
import Control.Applicative

-- ------------------------------------------------------
-- Deriving Lens

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
  deriving Show

intTree 
  = Node 2 (Leaf 3) 
           (Node 5 (Leaf 7) 
                   (Leaf 11))

listTree 
  = Node [1,1] (Leaf [2,1]) 
               (Node [3,2] (Leaf [5,2]) 
                           (Leaf [7,4]))

tupleTree 
  = Node (1,1) (Leaf (2,1)) 
               (Node (3,2) (Leaf (5,2)) 
                           (Leaf (7,4)))

-- ------------------------------------------------------

getRoot :: Tree a    -> a
getRoot (Leaf z)     = z
getRoot (Node z _ _) = z

setRoot :: Tree a -> a -> Tree a
setRoot (Leaf _)     x = Leaf x
setRoot (Node _ l r) x = Node x l r

main1 = do
  print $ getRoot intTree
  -- 2
  print $ setRoot intTree 11
  -- Node 11 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
  print $ getRoot (setRoot intTree 11)
  -- 11

-- ------------------------------------------------------
-- naive get/set...
fmapRoot f tree = setRoot tree newRoot
  where newRoot = f (getRoot tree)

-- fmapRoot with single single traversal
fmapRoot' :: (a -> a) -> Tree a -> Tree a
fmapRoot' f (Leaf z)     = Leaf (f z)
fmapRoot' f (Node z l r) = Node (f z) l r

-- setRoot i.t.o fmapRoot
setRoot' :: Tree a -> a -> Tree a
setRoot' tree x = fmapRoot' (\_ -> x) tree

main2 = do
  print $ setRoot' intTree 11
  -- Node 11 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
  print $ fmapRoot' (*2) intTree
  -- Node 4  (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

-- ------------------------------------------------------

{- 
    fmapRoot'  :: (a ->    a) -> Tree a ->     Tree a
    fmapRootIO :: (a -> IO a) -> Tree a -> IO (Tree a)
    fmapM      :: (a -> m  a) -> Tree a -> m  (Tree a)
-}

-- type Lens' s a = Functor f' => (a -> f' a) -> s -> f' s

-- ------------------------------------------------------

{- 
  lens'       :: Functor f  => (a -> f' a) -> s      -> f' s
  root        :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)
  fmapRootIO  ::               (a -> IO a) -> Tree a -> IO (Tree a)
-}

fmapRootIO :: (a -> IO a) -> Tree a -> IO (Tree a)
fmapRootIO g (Leaf z)     = (g z) >>= return . Leaf
fmapRootIO g (Node z l r) = (g z) >>= return . (\x -> Node x l r)

displayM x = print x >> return x  
main3 = fmapRootIO displayM intTree
{- 
  2
  Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
-}

-- ------------------------------------------------------

-- If we drop down from the IO Monad into Functor we have a root lens:

-- fmapRootIO ::      (a -> IO a) -> Tree a -> IO (Tree a)
root :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)

root g (Node z l r) = fmap (\x -> Node x l r) (g z)
root g (Leaf z)     = fmap (\x -> Leaf x)     (g z)

main4 = root displayM intTree
{- 
  2
  Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
-}

-- ------------------------------------------------------

main5 = do
  -- GET
  print $ view root listTree
  -- [1,1]
  print $ view root intTree
  -- 2

  -- SET
  print $ set root [42] listTree
  -- Node [42] (Leaf [2,1]) (Node [3,2] (Leaf [5,2]) (Leaf [7,4]))
  print $ set root 42   intTree
  -- Node 42 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

  -- FMAP (deliver function to lens focus)
  print $ over root (+11) intTree
  -- Node 13 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

-- ------------------------------------------------------
-- Composable getters and setters

rightMost :: Functor f' => 
  (a -> f' a) -> Tree a -> f' (Tree a)

rightMost g (Node z l r) 
  = fmap (\r' -> Node z l r') (rightMost g r)
rightMost g (Leaf z)     
  = fmap (\x -> Leaf x) (g z)

{- 
  tupleTree 
    = Node (1,1) (Leaf (2,1)) 
                 (Node (3,2) (Leaf (5,2)) 
                             (Leaf (7,4)))
-}
main6 = do --2
  print $ view  rightMost             tupleTree
  -- (7,4)

  print $ set   rightMost (0,0)       tupleTree
  -- Node (1,1) (Leaf (2,1)) 
  --            (Node (3,2) (Leaf (5,2)) 
  --                        (Leaf (0,0)))

  -- Compose Getters and Setters
  print $ view  (rightMost._1)        tupleTree
  -- 7

  print $ set   (rightMost._1) 0      tupleTree
  -- Node (1,1) (Leaf (2,1)) 
  --            (Node (3,2) (Leaf (5,2)) 
  --                        (Leaf (0,4)))

  print $ over  (rightMost._1) (*100) tupleTree
  -- Node (1,1) (Leaf (2,1)) 
  --            (Node (3,2) (Leaf (5,2)) 
  --                        (Leaf (700,4)))


-- ------------------------------------------------------
-- Lens Traversal

{- 
  lens      :: Functor f'     => (a -> f' a) -> Tree a -> f' (Tree a)
  traversal :: Applicative f' => (a -> f' a) -> Tree a -> f' (Tree a)
-}

leaves :: Applicative f' => (a -> f' a) -> Tree a -> f' (Tree a)
leaves g (Node z l r) 
  = Node z <$> leaves g l <*> leaves g r
leaves g (Leaf z)     
  = Leaf <$> (g z)
-- deliver function g to only to Leafs

-- Traversals compose seamlessly with Lens, Getters, and Setters:
main7 = do 
  -- Set and fmap over Traversal
  print $ set leaves 0 intTree
  -- Node 2 (Leaf 0) (Node 5 (Leaf 0) (Leaf 0))

  print $ over leaves (+1) intTree
  -- Node 2 (Leaf 4) (Node 5 (Leaf 8) (Leaf 12))
  
  -- Compose Traversal + Lens
  print $ over (leaves._1) (*100) tupleTree
  -- Node (1,1) (Leaf (200,1)) (Node (3,2) (Leaf (500,2)) (Leaf (700,4)))

  
  -- Compose Traversal + Traversal
  -- ('both' is a Tuple traversal that focusses on both elements)
  print $ over (leaves.both) (*100) tupleTree
  -- Node (1,1) (Leaf (200,100)) (Node (3,2) (Leaf (500,200)) (Leaf (700,400)))

  -- map over each elem in target container (e.g. list)
  print $ over (leaves.mapped) (*(-1)) listTree
  -- Node [1,1] (Leaf [-2,-1]) (Node [3,2] (Leaf [-5,-2]) (Leaf [-7,-4]))

  -- Traversal with IO effects
  mapMOf_ leaves displayM tupleTree
  -- (2,1)
  -- (5,2)
  -- (7,4)

-- ------------------------------------------------------
-- Lens.Fold

main8 = do
  print $ sumOf leaves intTree
  -- 21

  print $ anyOf leaves (>0) intTree
  -- True
  print $ anyOf (leaves._1) (<0) tupleTree
  -- False

  print $ foldMapOf (leaves._1) Sum tupleTree
  -- Sum {getSum = 14}
  print . getSum $ foldMapOf (leaves._1) Sum tupleTree
  -- 14
