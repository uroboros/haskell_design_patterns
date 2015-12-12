{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeFamilies #-}

-- ------------------------------------------------------
--  List' and its type representation RList (from Chapter 6)

data List' a 
  = Nil' | Cons' a (List' a)  deriving Show

data U = U                    deriving Show
data Choice a b = L a | R b   deriving Show
data Combo a b = Combo a b    deriving Show

type RList a = Choice U (Combo a (List' a))

-- ------------------------------------------------------
-- using Functional Dependencies

{- instead of ... 

  fromL :: List' a -> RList a
  toL   :: RList a -> List' a

  data EP d r = EP {from_ :: (d -> r),
                    to_   :: (r -> d)}

-}

class GenericFD d r | d -> r where
  from :: d -> r
  to   :: r -> d

-- higher-kinded multi-parameter type-class instance
instance GenericFD (List' a) (RList a) where
  from Nil'           = L U
  from (Cons' x xs)   = R (Combo x xs)

  to (L U)            = Nil'
  to (R (Combo x xs)) = (Cons' x xs)

{- 
  from :: GenericFD d r => d -> r
  
  e.g.
    from (Cons' "1" Nil') :: RList [Char]
    from (Cons' 1 Nil') :: Num a => RList a
-}

main1 = print $ from (Cons' "1" Nil')
        -- R (Combo "1" Nil')

-- ------------------------------------------------------
-- using Associated type synonyms

-- {-# LANGUAGE TypeFamilies #-}

class GenericA d where
  type Rep d :: *
  
  fromA :: d        -> (Rep d)
  toA   :: (Rep d)  -> d

instance GenericA (List' a) where
  type Rep (List' a) = (RList a)
  -- associated type params must match the class params
  
  fromA Nil'          = L U
  fromA (Cons' x xs)  = R (Combo x xs)
  
  toA (L U)            = Nil'
  toA (R (Combo x xs)) = (Cons' x xs)


main2 = print $ fromA (Cons' 1 Nil')
        -- R (Combo 1 Nil')

-- ------------------------------------------------------
-- Type Synonym Families

type family RepF d 
type instance RepF (List' a) = (RList a)

class GenericF d where
  fromF :: d        -> (RepF d)
  toF   :: (RepF d) -> d

instance GenericF (List' a) where
  fromF Nil'           = L U
  fromF (Cons' x xs)   = R (Combo x xs)

  toF (L U)            = Nil'
  toF (R (Combo x xs)) = (Cons' x xs)

main3 = print $ fromF (Cons' 1 Nil')
        -- R (Combo 1 Nil')

-- ------------------------------------------------------
-- Data Families

{-  
  -- associated data family
    class GMap k where
      data GMap k :: * -> *

      empty       :: GMap k v
      lookup      :: k -> GMap k v -> Maybe v 
      insert      :: k -> v -> GMap k v -> GMap k v

  -- or as top-level data family...

    data family GMap k :: * -> * -- 
    ...
-}
