{-# LANGUAGE PolyKinds #-}

-- ------------------------------------------------------

{- multiple type-classes to deal with multiple kind-orders

  class Typeable (a :: * ) where 
    typeOf :: a -> TypeRep

  class Typeable1 (a :: * -> *) where 
    typeOf1 :: forall b. a b -> TypeRep

-}

class T0 a where
  f0 :: a -> String

instance T0 Int where
  f0 _ = "T0 Int"

instance T0 Char where
  f0 _ = "T0 Char"

-- f0 (10::Int)
--   "T0 Int"
-- f0 'x'
--   "T0 Char"

instance T0 (Maybe a) where
  f0 _  = "T0 Maybe a"

class T1 m where -- m :: * -> *
  f1 :: Show a => m a -> String

instance T1 Maybe where
  f1 _ = "T1 Maybe"

-- f1 (Just 10)
--   "T1 Maybe"

-- ------------------------------------------------------
-- Polymorphic kinds allow us to unify the type-classes T0 and T1 into one

class T a where -- (a::k)
  f :: Proxy a -> String

{-
  With the DataKinds language extension, k is polymorphic by default
  i.e. k can take several forms (of kind signatures)
  -- *
  -- * -> *
  -- * -> * -> *

  k is a polymorphic placeholder for many possible kind arities.
-}

-- Proxy is a phantom-type
data Proxy a = Proxy deriving Show -- (a::k)
{- 
    'a' is of polymorphic kind e.g.

    (Proxy Int)    -- Proxy :: *        -> *
    (Proxy Maybe)  -- Proxy :: (* -> *) -> *
-}

-- Proxy is used to generalise the kind of the first argument of f
--   f :: T a => Proxy a -> String

{- 
  By enabling Polykinds the kind signatures generalize:
  
  -- before and after PolyKinds
  f  :: *           -> Constraint
  f  :: forall k. k -> Constraint

  -- before and after PolyKinds
  Proxy :: *           -> *
  Proxy :: forall k. k -> *

-}

-- We can verify that the type parameter has polymorphic kind: 

instance T Int where -- Int :: *
  f _ = "T Int"

instance T Maybe where -- Maybe :: * -> *
  f _  = "T Maybe"

-- f (Proxy :: Proxy Maybe) -- "T Maybe"
-- f (Proxy :: Proxy Int)   -- "T Int"
