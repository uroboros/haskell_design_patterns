import System.IO
import Control.Monad
import Control.Applicative

-- Imperative style
main1 = do 
  h <- openFile "jabberwocky.txt" ReadMode

  line  <- hGetLine h

  putStrLn . show . words $ line
  -- ["'Twas","brillig,","and","the","slithy","toves"]
  hClose h

-- vs Monadic style (using >>=)

main2 = do 
  h <- openFile "jabberwocky.txt" ReadMode

  hGetLine h >>= print . words

  -- ["'Twas","brillig,","and","the","slithy","toves"]
  hClose h


-- IO as FUNCTOR  
main3 = do
  h <- openFile "jabberwocky.txt" ReadMode

  line <- fmap (show . words) (hGetLine h)

  putStrLn line
  -- ["'Twas","brillig,","and","the","slithy","toves"]
  hClose h

-- IO as APPLICATIVE
main4 = do
  h <- openFile "jabberwocky.txt" ReadMode

  line <- (show . words) <$> (hGetLine h)

  putStrLn line
  -- ["'Twas","brillig,","and","the","slithy","toves"]
  hClose h


-- IO as MONAD
main5 = do
  h <- openFile "jabberwocky.txt" ReadMode

  line <- liftM (show . words) (hGetLine h)

  putStrLn line
  -- ["'Twas","brillig,","and","the","slithy","toves"]
  hClose h

