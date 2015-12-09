import Control.Monad

-- Arrow
import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow

import System.IO

-- ------------------------------------------------------

main1 = liftM (length . words) (readFile "jabberwocky.txt" ) 
          >>= print
        -- 66  
 -- regular functions: length, words
 -- Monadic functions: readFile, print

-- print . length . words . readFile "jabberwocky.txt"
-- -> INVALID - types don't align

-- ------------------------------------------------------
-- baby Arrow

data IOF a b = IOF {runIOF :: a -> IO b}

-- Composition operator
(<<<<) :: IOF a b -> IOF c a -> IOF c b
(IOF f) <<<< (IOF g) = IOF $ f <=< g

-- lifting function
lift' :: (a -> b) -> IOF a b
lift' f = IOF $ return . f -- uses IO Monad's return

-- Now we can compose regular and IO functions:
main2 = do
  let f =  IOF print <<<< lift' length <<<< lift' words <<<< IOF readFile
  -- vs        print   .     length      .      words     .    readFile
  runIOF f "jabberwocky.txt"
  -- 166
  return ()

-- ------------------------------------------------------
-- Implementing an arrow

data IOArrow a b = IOArrow {runIOArrow :: a -> IO b}

instance Category IOArrow where
  id = IOArrow return
  -- (.) = (<<<<)
  IOArrow f . IOArrow g = IOArrow $ f <=< g

instance Arrow IOArrow where
  -- arr = lift'
  arr f = IOArrow $ return . f 

  first (IOArrow f) = IOArrow $ \(a, c) -> do
    x <- f a
    return (x, c)

main3 = do
  let f = IOArrow print . arr length . arr words . IOArrow readFile
  -- same as...
  let g = IOArrow print <<< arr length <<< arr words <<< IOArrow readFile
  -- same as...
  let h = (IOArrow readFile) >>> (arr words) >>> (arr length) >>> (IOArrow print)

  runIOArrow f "jabberwocky.txt"
  runIOArrow g "jabberwocky.txt"
  runIOArrow h "jabberwocky.txt"
  -- 166

-- ------------------------------------------------------
-- Arrow Operators

{- INVALID sketch code...

main = do
  let f = (IOArrow readFile) >>>
          (arr words) >>>
          (arr (\x -> (x,x))) >>> -– split stream in 2 
          (arr length) >>> -- ERROR
          (IOArrow print)

    runIOArrow f "jabberwocky.txt"
-}

main4 = do
  let f = (IOArrow readFile) >>> 
          (arr words) >>> 
          (arr (\x -> (x,x))) >>>              -- "split" stream in 2
          (first (arr length)) >>>             -- act on first tuple val
          (IOArrow print)
  runIOArrow f "jabberwocky.txt"
  -- (166,["'Twas","brillig," ...])

main5 = do
  let f = (IOArrow readFile) >>> 
          (arr words) >>> 
          (arr (\x -> (x,x))) >>>              -- "split" stream in 2
          (first (arr length)) >>>             -- act on first tuple val
          (second (arr (length . concat))) >>> -- act on second tuple val 
          (IOArrow print)
  runIOArrow f "jabberwocky.txt"
  -- (166,762)


-- same as above, using *** Arrow operator
main6 = do
  let f = (IOArrow readFile) >>> 
          (arr words) >>> 
          (arr (\x -> (x,x))) >>>              -- "split" stream in 2
          (arr length *** arr (length . concat)) >>>          
          (IOArrow print)
  runIOArrow f "jabberwocky.txt"
  -- (166,762)

-- ------------------------------------------------------
-- Kleisli Arrows

{- (already defined in Control.Arrow)

   data Kleisli m a b = K {runKleisli :: a -> m b}

   instance Monad m => Arrow (Kleisli m) where
     arr f        = K (\x -> return (f x))
     K f >>> K g  = K (\x -> f x >>= g)
-}

main7 = do
  let f = Kleisli print . arr length . arr words . Kleisli readFile
  -- vs   IOArrow print . arr length . arr words . IOArrow readFile

  runKleisli f "jabberwocky.txt"
  -- 166
