{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- ------------------------------------------------------
-- Peano-numbering example

data Zero = Zero deriving Show
data Succ n = Succ n deriving Show

one = Succ Zero
two = Succ one

-- We'll use this with the understanding that certain "bad" expressions 
-- are allowed:
badSucc1 = Succ 10    -- :: Succ Int
badSucc2 = Succ False -- :: Succ Bool

-- Our size-aware list type Vec is represented as a GADT:

-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
data Vec :: (* -> * -> *) where
  Nil  :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Succ n)

nil' = Nil :: Vec Int Zero
cons1 = Cons 3 nil' --  :: Vec Int (Succ Zero)
cons2 = Cons 5 cons1 -- :: Vec Int (Succ (Succ Zero))

-- Unfortunately this is valid:
badVec = Nil :: Vec Zero Zero
-- but at least not 

-- badVec2 = Nil :: Vec Zero Bool -- INVALID

-- We need more type-safety on the kind-level, and for that we need data-types on the 
-- kind level. That is precisely what the DataKinds extension is for.

-- ------------------------------------------------------
-- Promoting types to kinds

-- {-# LANGUAGE DataKinds #-}
data Nat = ZeroD | SuccD Nat

{- This gives us more type-safety

    badSuccD = SuccD 10 -- INVALID
-}

-- Datakinds will automatically promote the Nat _type_ to the 
-- Nat _kind_. The data-constructors ZeroD and SuccD are promoted to _types_

-- The revised Vec uses datakinds along with kind polymorphism.
-- The second type parameter is now constrained to be of _kind_ Nat.

data VecD :: * -> Nat -> * where
  NilD :: VecD a 'ZeroD
  ConsD :: a -> VecD a n -> VecD a ('SuccD n)

cons1D = ConsD 3 NilD --   :: VecD Integer ('SuccD 'ZeroD)
cons2D = ConsD '5' NilD -- :: VecD Char ('SuccD 'ZeroD)

-- ------------------------------------------------------
-- (Typed) Type-level programming

vappend :: VecD e n -> VecD e m -> VecD e (Add n m)
vappend NilD         l   = l
vappend (ConsD x xs) ys  = ConsD x (vappend xs ys)

-- Add is a type level function that adds two type-level numbers.
-- We can express this as a type family

type family Add (n :: Nat) (m :: Nat) :: Nat

type instance Add ZeroD m = m
type instance Add (SuccD n) m = SuccD (Add n m)

-- vappend two Vec's:
xs = vappend (ConsD 3 (ConsD 5 NilD))
             (ConsD 7 NilD)

-- xs :: VecD Integer ('SuccD ('SuccD ('SuccD 'ZeroD)))

------------------------------------------------------
-- Closed type-families

type family IsZero (n::Nat) :: Bool where
  IsZero 'ZeroD = 'True
  IsZero ('SuccD n) = 'False
