import Data.Maybe
import Control.Applicative

-- ------------------------------------------------------

{- Applicative inherits from Functor:

  class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

-- ------------------------------------------------------

data Maybe' a = Just' a | Nothing'
  deriving Show

instance Functor Maybe' where
  fmap _ Nothing'  =  Nothing'
  fmap f (Just' x) =  Just' (f x)

instance Applicative Maybe' where
  pure f = Just' f

  Nothing'  <*> _         = Nothing'
  _         <*> Nothing'  = Nothing'
  (Just' f) <*> (Just' x) = Just' (f x)

main1 = do
  print $ pure (,) <*> Just' 2 <*> Just' 3
  -- Just' (2,3)

  -- same as...
  print $ (fmap (,) (Just' 2)) <*> Just' 3 
  
  -- same as: (<$> shorthand for Applicative fmap)
  print $ (,) <$> (Just' 2) <*> Just' 3
  
  -- LAW of Composition  
  print $ Just' (.) <*> Just' (+2) <*> Just' (+3) <*> Just' 1
  -- same as...
  print $ Just' (+2) <*> (Just' (+3) <*> Just' 1)
  -- Just' 6

{- 
  fmap, (<$>) ::   (a -> b) -> f a -> f b
        (<*>) :: f (a -> b) -> f a -> f b
-}
