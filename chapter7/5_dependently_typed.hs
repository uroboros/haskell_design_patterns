{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} --  Nested type family application

------------------------------------------------------

{- 
    sprintf "Name=%s"         :: String -> String
    sprintf "Age=%d"          :: Int -> String
    sprintf "Name=%s, Age=%d" :: String -> Int -> String

    -- the type of (sprintf fs) depends on the value of the format descriptor fs
-}

data L
data V val

data F t where
     Lit   ::          String -> F L
     Val   :: (val -> String) -> F (V val)

type family SPrintf f
type instance SPrintf L       = String
type instance SPrintf (V val) = val -> String

sprintf :: F f -> SPrintf f
sprintf (Lit str)  = str
sprintf (Val show') = \x -> (show' x)

main = do
  print $ sprintf (Val show) 12
  -- same as
  print $ sprintf (Val (show::Int -> String)) 12
  -- "12"

  -- INVALID
  -- print $ sprintf (Val (show::Int -> String)) "12"

  print $ sprintf (Val show) [12, 13, 14]
  -- same as
  print $ sprintf (Val (show::[Int] -> String)) [12, 13, 14]
  -- "[12,13,14]"

  print $ sprintf (Lit "hello") -- "hello"
  -- "hello"

-- The type of the first argument of sprintf determines the return type of sprintf.
