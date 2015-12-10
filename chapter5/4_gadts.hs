{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

-- ------------------------------------------------------
-- a GADT with phantom type 't' and built-in smart constructors
data Expr t where 
    I   :: Int  -> Expr Int    
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int

-- with GADTs we can pattern match on the smart constructors
-- (this was not possible with Phantom types)
eval :: Expr t -> t
eval (I v) = v
eval (B v) = v
eval (Add x y) = (eval x) + (eval y)

main1 = do
  print $ eval (Add (I 10) (I 12))

  -- INVALID (rejected by compiler)
  -- print $ eval (Add (I 10) (B True))

-- ------------------------------------------------------
-- Type-case Pattern with GADTs

data Rep t where
    RInt  ::                    Rep Int
    RChar ::                    Rep Char
    RList :: Show a => Rep a -> Rep [a]

-- ...a function which takes a value along with its type representation:
showT :: Show t => Rep t -> t -> String

showT RInt i  = (show i) ++ " :: INT"
showT RChar i = (show i) ++ " :: Char"

showT (RList rep) [] = "THE END"
showT (RList rep) (x:xs) 
  = (showT rep x) ++ ", " 
    ++ (showT (RList rep) xs)

-- showT is a (closed) "type-indexed function" because it is defined for each member of 
-- the family of types Rep t:

main2 = do
  print $ showT RInt 3
  -- "3 :: INT"
  print $ showT (RList RInt)  [12,13,14]
  -- "12 :: INT, 13 :: INT, 14 :: INT, THE END"
  print $ showT (RList RChar) ['2','3','5']
  -- "'2' :: Char, '3' :: Char, '5' :: Char, THE END"

-- ------------------------------------------------------
-- Dynamic Types: using Existential quantification

-- Use Existential type to package a type together with the type representation:
data DynamicEQ = forall t. Show t => 
                    DynEQ (Rep t) t

-- heterogeneous list using a Dynamic type
dynEQList = [DynEQ RChar 'x', DynEQ RInt 3]

-- ------------------------------------------------------
-- Dynamic Types: using GADTs

data Dynamic where 
  Dyn :: Show t => Rep2 t -> t -> Dynamic

-- Redefine Rep -> Rep2, showT -> showT2 >>>>>>>>>>>
data Rep2 t where
    RInt2  ::                     Rep2 Int
    RChar2 ::                     Rep2 Char
    RList2 :: Show a => Rep2 a -> Rep2 [a]
    RDyn   ::                     Rep2 Dynamic -- NEW constructor

showT2 :: Show t => Rep2 t -> t -> String

showT2 RInt2  i = (show i) ++ " :: INT"
showT2 RChar2 i = (show i) ++ " :: Char"

showT2 (RList2 rep) [] = "THE END"
showT2 (RList2 rep) (x:xs) 
  = (showT2 rep x) ++ ", " 
    ++ (showT2 (RList2 rep) xs)

showT2 RDyn (Dyn rep v) = showT2 rep v -- NEW clause for RDyn constructor
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

instance Show Dynamic where
  show (Dyn rep v) = showT2 rep v

main3 = do 
  print $ showT2 RInt2 17
  print $ showT2 (RList2 RInt2) [12,13,14]
  print $ showT2 (RList2 RDyn) dynList

-- heterogeneous list using GADT
dynList :: [Dynamic]
dynList = [Dyn RChar2 'x', Dyn RInt2 3]

showDyn (Dyn rep v) = showT2 rep v

main4 = mapM_ (putStrLn . showDyn) dynList
        -- 'x' :: Char
        -- 3 :: INT

toInt :: Dynamic -> Maybe Int
toInt (Dyn RInt2 i) = Just i
toInt (Dyn _ _)    = Nothing

-- ------------------------------------------------------
-- Heterogeneous Lists: using Existentials

-- Existential Type v1 ........................
data LI_Eq1 = forall  a. LI_Eq1 a 

hListEq1 :: [LI_Eq1]
hListEq1 = [LI_Eq1 3, LI_Eq1 "5"]

-- Existential Type v2 ........................
-- package a show function with each item

data LI_Eq2 = forall  a. LI_Eq2 a (a -> String)  -- Existential Type

hListEq2 :: [LI_Eq2]
hListEq2 = [LI_Eq2  3  (show :: Int -> String), 
            LI_Eq2 "5" (show :: String -> String)]
-- (the 'show' type signatures can be inferred i.e. ommited)

showEq2 (LI_Eq2 v showF) = showF v
-- e.g. main = mapM_ (putStrLn . showEq2) hListEq2

-- Existential Type v3 ........................
-- "bounded quantification" - in this case bounded by the Show type-class
data LI_Eq3 = forall a. Show a => LI_Eq3 a

hListEq3 :: [LI_Eq3]
hListEq3 = [LI_Eq3  3, LI_Eq3 "5"]
showEq3 (LI_Eq3 v) = show v

main5 = mapM_ (putStrLn . showEq3) hListEq3

-- ------------------------------------------------------
-- Heterogeneous Lists Using GADTS

-- GADT v1 ........................
-- passing in a function 
data LI_Gadt1 where
  {MkShow1 :: a -> (a -> String) -> LI_Gadt1}

--hListGadt1 :: [LI_Gadt1]
hListGadt1 = [MkShow1 "3" show, MkShow1 5 show]
showGadt1 (MkShow1 v showF) = showF v

main6 = mapM_ (putStrLn . showGadt1) hListGadt1 

-- GADT v2 ........................
-- bounded quantification

data LI_Gadt2 where
  {MkShow2 :: Show a => a -> LI_Gadt2}

hListGadt1 :: [LI_Gadt1]
hListGadt2 = [MkShow2 "3", MkShow2 5]
showGadt2 (MkShow2 v) = show v

main7 = mapM_ (putStrLn . showGadt2) hListGadt2 

