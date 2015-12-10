-- ------------------------------------------------------

data Expr1 = I1 Int |
             Add1 Expr1 Expr1
-- e.g. I1 3
--      (Add1 (I1 3) 
--            (Add1 (I1 5) (I1 7)))

eval1 :: Expr1 -> Int
eval1 (I1 v) = v
eval1 (Add1 x y) = (eval1 x) + (eval1 y)

main1 = do
  print $ eval1 (I1 7)
  -- 7
  print $ eval1 (Add1 (I1 7) (I1 5))
  -- 12

-- ------------------------------------------------------
-- The problem...

data Expr2 = I2 Int |
             B2 Bool |
             Add2 Expr2 Expr2
             deriving Show

eval2 :: Expr2 -> Int -- should be "Int or Bool"
eval2 (I2 v) = v
-- eval2 (B2 v) = v -- INVALID

eval2 (Add2 x y) = (eval2 x) + (eval2 y)
-- how do we constrain the Add2 type args to be of type I2,
-- and not B2, e.g.
--   eval2 (Add2 (I2 7) (B2 True))

main2 = do
  print $ eval2 (Add2 (I2 7) (I2 5))

  -- INVALID!
  -- print $ eval2 (Add2 (I2 7) (B2 True)) 

-- ------------------------------------------------------
-- The (partial) solution: Phantom types

-- t serves as a type "placeholder"
data Expr3 t =  I3 Int |
                B3 Bool |
                Add3 (Expr3 Int) (Expr3 Int)
  deriving Show

-- The constructors all still return the same type: 

{- 
  I3   ::                    Int -> Expr3 t
  B3   ::                   Bool -> Expr3 t
  Add3 :: Expr3 Int -> Expr3 Int -> Expr3 t
-}

-- We can still construct invalid values:
main3 = do
  print $ Add3 (I3 11) (B3 True)

-- ------------------------------------------------------

-- type-safe construction with "smart constructors":
i3 :: Int   -> Expr3 Int
i3 = I3
b3 :: Bool  -> Expr3 Bool
b3 = B3

add3 :: Expr3 Int -> Expr3 Int -> Expr3 Int
add3 = Add3

-- If we use the smart constructors instead of data-type constructors,
-- the Haskell type-checker will prevent us from creating 

-- If we use smart constructors instead of data-type constructors,
-- the type-checker can reject "bad" expressions
main4 = do
  -- Allowed by compiler
  print $ Add3 (I3 7) (B3 True)
  -- Add3 (I3 7) (B3 True)
  print $ add3 (i3 10) (i3 7)
  -- Add3 (I3 10) (I3 7)

  -- rejected by compiler 
  -- print $ add3 (i3 10) (b3 True) -- INVALID
  {- 
    "Couldn't match type ‘Bool’ with ‘Int’
      Expected type: Expr3 Int
      Actual type: Expr3 Bool"
  -}

-- ------------------------------------------------------

-- Phantom types with smart constructors enable type-safe construction,
-- but type inference remains a problem:

-- Because values are still not described accurately enough, e.g.
-- (I3 12) :: Expr3 t   -- this 
--         :: Expr3 Int -- instead of

-- adding values remains ambiguous:
--   eval3 (Add3 x y) = (eval3 x) + (eval3 y)

