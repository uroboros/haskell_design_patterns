{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- ------------------------------------------------------
-- Multiparameter type-classes

class Coerce a b where
  coerce :: a -> b

instance Coerce Int String where
  coerce = show

instance Coerce Int [Int] where
  coerce x = [x]

-- ghci> (coerce (12::Int)) :: String
--      "12"

-- ghci> (coerce (12::Int)) :: [Int]
--      [12]

-- ------------------------------------------------------
-- Functional dependencies
 
class Coerce2 a b | b -> a where
  coerce2 :: a -> b

instance Coerce2 Int String where
  coerce2 = show

instance Coerce2 Int [Int] where
  coerce2 x = [x]

-- ghci> (coerce2 12) :: String
--      "12"

-- ghci> (coerce2 12) :: [Int]
--      [12]

{- ERROR: "Functional dependencies conflict between instance declarations"

  instance Coerce2 Float String where
     coerce2 = show 
-}



