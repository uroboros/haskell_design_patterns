import Control.Applicative
import Control.Monad

-- ------------------------------------------------------

{- 
  class (Applicative m) => Monad m where 
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
-}

-- ------------------------------------------------------

data Maybe' a = Just' a | Nothing'
  deriving Show

-- we can base our Functor implementation of Maybe' on it's Monadic properties
instance Functor Maybe' where
  fmap = liftM

{- vs...

  instance Functor Maybe' where
    fmap _ Nothing' =   Nothing'
    fmap f (Just' x) =  Just' (f x)
-}

-- similarly, for Applicative:

instance Applicative Maybe' where
  pure = return
  (<*>) = ap -- (see below...)

{- vs...

  instance Applicative Maybe' where
    pure f = Just' f
    Nothing'  <*> _         = Nothing'
    _         <*> Nothing'  = Nothing'
    (Just' f) <*> (Just' x) = Just' (f x)
-}  

instance Monad Maybe' where
  return x = Just' x

  Nothing'  >>= _  = Nothing'
  (Just' x) >>= f  = (f x)

main1 = do
  print $ Just' 10 >>= \x -> Just' (show x)
  -- Just' "10"
  print $ Nothing' >>= \x -> Just' (x * 2)
  -- Nothing'

-- ------------------------------------------------------
-- MONAD as FUNCTOR

{- liftM is the fmap of Monad...

  liftM :: Monad  m => (a -> b) -> m a -> m b 
  fmap :: Functor m => (a -> b) -> m a -> m b
-}

main2 = do
  -- 3 levels of expressing fmap: Functor -> Applicative -> Monad
  print $ fmap  (*2)     (Just' 10) -- FUNCTOR 
  print $ pure  (*2) <*> (Just' 10) -- APPLICATIVE 
  print $ liftM (*2)     (Just' 10) -- MONAD
  -- Just' 20

-- ------------------------------------------------------
-- MONAD as APPLICATIVE

main3 = do
  print $ (<$>)  (*) (Just' 10) <*> (Just' 20) -- APPLICATIVE
  -- Just' 200
  print $ liftM2 (*) (Just' 10)     (Just' 20) -- MONAD
  -- Just' 200
  print $ liftM3 f3  (Just' 10)     (Just' 20) (Just' 30)
  -- Just' 6000
  where f3 x y z = x * y * z

-- ap_ defines <*> for Monads
-- (already defined in Control.Monad.ap)
ap_ mf mx = do
 f <- mf      -- perform mf action, extract function
 x <- mx      -- perform mf action, extract val
 return (f x) -- apply f to x

main4 = do
  print $ (Just' (*)) `ap_` (Just' 10) `ap_` (Just' 20)
  -- Just' 200

{- 
  instance Applicative Maybe' where 
    pure  = return
    (<*>) = ap
-}

-- ------------------------------------------------------
-- Sequencing actions with Monad and Applicative

action s = do putStrLn s; return s
main5 = do
  let actions = map action ["the", "parts", "are", "disconnected"]
  sequence' actions
  return ()

-- where sequence performs the actions one after the other:
sequence' [] = return []
sequence' (x:xs) = do
  x'  <- x -- action performed
  xs' <- sequence' xs
  return (x':xs')

-- But we can also sequence actions with Applicative
-- (defined in Prelude.sequenceA)
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> (sequenceA' xs)

main6 = do
  let actions = map action ["the", "parts", "are", "disconnected"]
  sequenceA' actions
  return ()

-- ------------------------------------------------------
-- Monad as Applicative

-- cannot do this with Applicative...
main7 = do
     line <- getLine                -- ACTION 1
     putStrLn $ "You said " ++ line -- ACTION 2
                                    -- uses result of ACTION 1


main8 = mainLoop
mainLoop = do
  line <- getLine                       -- ACTION 1
  if line == "stop"
    then putStrLn "Bye"                 -- ACTION 2b
    else do
         putStrLn $ "You said " ++ line -- ACTION 2c
         mainLoop
