{-# LANGUAGE ExistentialQuantification #-}

-- ------------------------------------------------------
-- Universally Qualified data type

data ObjU a
    = ObjU a             -- property
           (a -> Bool)   -- obj method
           (a -> String) -- obj method

obj_f1 :: ObjU a -> Bool 
obj_f1 (ObjU v f1 _) = f1 v

obj_f2 :: ObjU a -> String 
obj_f2 (ObjU v _ f2) = f2 v

main1 = do
  print $ obj_f1 obj -- apply 'even' to 3 
  -- False
  print $ obj_f2 obj -- apply 'show' to 3 
  -- "3"
  where 
    obj = (ObjU 3 even show)

-- ------------------------------------------------------
-- Existentially Qualified data type: 

-- data ObjU a =           ObjU a (a -> Bool) (a -> String)
data    ObjE   = forall a. ObjE a (a -> Bool) (a -> String)
-- ObjE hides the type a (because it is absent on the left-hand-side of "data ObjE =")

-- type "a" is also not present in the type signatures acting on ObjE...
objE_f1 :: ObjE -> Bool 
objE_f1 (ObjE v f1 _) = f1 v
objE_f2 :: ObjE -> String 
objE_f2 (ObjE v _ f2) = f2 v


main2 = do
  print $ objE_f1 obj -- apply 'even' to 3 
  -- False
  print $ objE_f2 obj -- apply 'show' to 3 
  -- "3"
  where 
    obj = (ObjE 3 even show)

-- INVALID!
-- objE_f3 (ObjE v _ _) = v
-- (we cannot define functions that act on ObjE "from the outside", 
--  because the inner types are hidden)
