import Control.Monad

-- Monads ---------------------------------------------------------

data Expr = Lit Int | Div Expr Expr

eval :: Expr -> Int
eval (Lit a) = a
eval (Div a b) = eval a `div` eval b

main1 = do
  print $ eval (Lit 42)
  -- 42
  print $ eval (Div (Lit 44) (Lit 11))
  -- 4


data Try a = Err String | Return a
  deriving Show

evalTry :: Expr -> Try Int
evalTry (Lit a) = Return a
evalTry (Div a b) 
  = case (evalTry a) of
      Err e -> Err e
      Return a' -> case (evalTry b) of
                    Err e  -> Err e
                    Return b' -> divTry a' b'

divTry :: Int -> Int -> Try Int
divTry a b = if b == 0
    then Err "Div by Zero"
    else Return (a `div` b)

-- ERRATA (missing from book)
instance Functor Try where
  fmap f (Err e) = (Err e)
  fmap f (Return x) = (Return (f x))

-- ERRATA (missing from book)
instance Applicative Try where
  pure x = Return x

  (Err e)    <*> _          = Err e
  _          <*> (Err e)    = Err e
  (Return f) <*> (Return x) = (Return (f x))

instance Monad Try where
  return x  = Return x
  fail msg  = Err msg

  Err e  >>= _    = Err e
  Return a >>= f  = f a

evalTry' :: Expr -> Try Int
evalTry' (Lit a) = Return a
evalTry' (Div a b) = (evalTry' a) >>= \a' -> 
                      (evalTry' b) >>= \b' -> 
                        divTry a' b'

main2 = print $ evalTry' (Div (Lit 44) (Lit 0))

evalTry'' (Lit a) = Return a
evalTry'' (Div a b) = do
  a' <- (evalTry' a) 
  b' <- (evalTry' b) 
  divTry a' b'

main = do
  print $ evalTry'' (Div (Lit 44) (Lit 0))
  -- Err "Div by Zero"
  print $ evalTry'' (Div (Lit 44) (Lit 4))
  -- Return 11
