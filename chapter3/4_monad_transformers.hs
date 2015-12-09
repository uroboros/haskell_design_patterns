{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative

-- ------------------------------------------------------
-- We'll use a Reader monad to hold some configuration data
-- for an application:
data Config = Config {discountRate :: Float, 
                      currencySym :: String}

appCfg = (Config 10 "R") 

-- returns a Float, in the context of a 'Reader Config':
discount :: Float -> Reader Config Float
discount amt = do
  discountRate' <- asks discountRate
  return (amt * (1 - discountRate' / 100))
-- From within the 'discount' function, we can "ask" for the configuration data

-- Now we can 'runReader' with specific config data (appCfg):
main1 = do
  print $ runReader (discount 100) appCfg
  -- 90.0

-- ------------------------------------------------------

-- returns a String, in the context of a 'Reader Config':
display :: Float -> Reader Config String
display amt = do
  currencySym' <- asks currencySym
  return (currencySym' ++ " " ++ (show amt))

main2 = do
  putStrLn $ runReader doDoubleDiscount appCfg
  -- "R 81.0"
  where 
    doDoubleDiscount = (discount 100 >>= discount >>= display)
    -- same as...      (return 100 >>= discount >>= discount >>= display)

-- ------------------------------------------------------

discountWR :: Float -> ReaderT Config (Writer String) Float
discountWR amt = do
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ " > Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWR :: Float -> ReaderT Config (Writer String) String
displayWR amt = do
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

main3 = do
  print $ runWriter (runReaderT doDoubleDiscount appCfg) 
  -- ("R 81.0"," > Discount 100.0 = 90.0 > Discount 90.0 = 81.0 > Displaying...")
  where 
    doDoubleDiscount = (discountWR 100 >>= discountWR >>= displayWR)

-- ------------------------------------------------------

{- 
  discountWR :: Float -> ReaderT Config (Writer String) Float
  displayWR  :: Float -> ReaderT Config (Writer String) String
  runWriter (runReaderT someApp appCfg)
-}

-- simplify with ...
type App = ReaderT Config (Writer String)

{- 
  discountWR :: Float -> App Float
  displayWR  :: Float -> App String
-}

discountWR' :: Float -> App Float
discountWR' amt = do
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ " > Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWR' :: Float -> App String
displayWR' amt = do
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

doApp :: App a -> (a, String)
doApp app = runWriter (runReaderT app appCfg)

main4 = do
  print $ doApp doDoubleDiscount
  -- ("R 81.0"," > Discount 100.0 = 90.0 > Discount 90.0 = 81.0 > Displaying...")
  where 
    doDoubleDiscount = (discountWR' 100 >>= discountWR' >>= displayWR')

-- ------------------------------------------------------
-- more idiomatically, instead of 
--    type App = ReaderT Config (Writer String)
-- we use newtype:

newtype App2 a = App2 {runApp :: ReaderT Config (Writer String) a}
  deriving (Monad, Applicative, Functor, MonadReader Config, MonadWriter String)
-- requires {-# LANGUAGE GeneralizedNewtypeDeriving #-}

discountWR'' :: Float -> App2 Float
discountWR'' amt = do
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ " > Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWR'' :: Float -> App2 String
displayWR'' amt = do
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

doApp2 :: App2 a -> (a, String)
doApp2 app = runWriter (runReaderT (runApp app) appCfg)

main5 = do
  print $ doApp2 doDoubleDiscount
  -- ("R 81.0"," > Discount 100.0 = 90.0 > Discount 90.0 = 81.0 > Displaying...")
  where 
    doDoubleDiscount = (discountWR'' 100 >>= discountWR'' >>= displayWR'')

-- ------------------------------------------------------
-- IO in monad stacks

newtype AppIO a = AppIO {runAppIO :: ReaderT Config (WriterT String IO) a}
  deriving (Monad, Applicative, Functor, 
            MonadReader Config, MonadWriter String, MonadIO)

discountWRIO :: Float -> AppIO Float
discountWRIO amt = do
  liftIO $ putStrLn "We're doing IO in discountWRIO"
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ " > Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWRIO :: Float -> AppIO String
displayWRIO amt = do
  liftIO $ putStrLn "More IO in displayWRIO"
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

doAppIO :: AppIO a -> IO (a, String)
doAppIO app = runWriterT (runReaderT (runAppIO app) appCfg)

main6 = print <$> doAppIO doDoubleDiscount
        -- "We're doing IO in discountWRIO"
        -- "We're doing IO in discountWRIO"
        -- "More IO in displayWRIO"
  where 
    doDoubleDiscount = (discountWRIO 100 >>= discountWRIO >>= displayWRIO)

-- ------------------------------------------------------
-- Sequence of stack composition, reversed

newtype AppIO2 a = AppIO2 {runAppIO2 :: WriterT String (ReaderT Config IO) a}
  deriving (Monad, Applicative, Functor, 
            MonadReader Config, MonadWriter String, MonadIO)

discountWRIO2 :: Float -> AppIO2 Float
discountWRIO2 amt = do
  liftIO $ putStrLn "We're doing IO in discountWRIO"
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ " > Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWRIO2 :: Float -> AppIO2 String
displayWRIO2 amt = do
  liftIO $ putStrLn "More IO in displayWRIO"
  currencySym' <- asks currencySym
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

doAppIO2 :: AppIO2 a -> IO (a, String)
doAppIO2 app = runReaderT (runWriterT (runAppIO2 app)) appCfg

main7 = print <$> doAppIO2 doDoubleDiscount
        -- "We're doing IO in discountWRIO"
        -- "We're doing IO in discountWRIO"
        -- "More IO in displayWRIO"
  where 
    doDoubleDiscount = (discountWRIO2 100 >>= discountWRIO2 >>= displayWRIO2)
