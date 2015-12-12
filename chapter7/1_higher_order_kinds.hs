-- ------------------------------------------------------
-- Types classify Values

-- Primitive types
-- "a string"   :: String
-- 12           :: Int

-- instances of higher-order, parametrized types
-- Just 10      :: Maybe Int
-- Left 10      :: Either Int b

-- functions are first class values
-- (* 2)        :: Num a => a -> a

-- type-constructors are functions
-- Just         :: a -> Maybe a
-- Left         :: a -> Either a b

-- ------------------------------------------------------
-- Kinds classify types

-- For monomorphic types the kind signature is just the placeholder, "*":

-- TYPE         KIND
-- [Char]     ::  *
-- Maybe Int  ::  *

-- Parametric types express higher-order kinds, e.g

-- Maybe  ::  * -> *
--            a -> Maybe a

-- Either       * -> * -> *
--              a -> b -> Either a b

-- Kinds can distinguish only lifted types (of kind *), 
-- Type Constructors (e.g. * -> * -> *) and "raw" unboxed types.


-- ------------------------------------------------------
-- Typeclasses and higher-kinded polymorphism

--   class Show a :: * -> *
--                   a -> Show a

--   instance (Show a) => Show (Maybe a) where ...

-- the typeclass parameters in the kind signatures need to be aligned: we use
-- Maybe' b :: * instead of Maybe :: (* -> *) in order to match the kind of a :: *.

-- class Monad m :: (* -> *) -> *
--                     m     -> Monad m

--   instance             Monad Maybe     where ...
--   vs 
--   instance (Show a) => Show  (Maybe a) where ...
