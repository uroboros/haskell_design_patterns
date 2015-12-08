import System.IO
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr)

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8

main1 = do
  let ios = map putStrLn ["this", "won't", "run"]
  putStrLn "until ios is 'sequenced'..."
  sequence_ ios

-- ------------------------------------------------------

main2 = do 
  h <- openFile "jabberwocky.txt" ReadMode

  line1 <- hGetLine h                 -- perform action
  let getLines = [hGetLine h, hGetLine h]
  [line2, line3] <- sequence getLines -- perform actions

  hClose h
  putStrLn line1
  putStrLn line2
  putStrLn line3

-- ------------------------------------------------------

main3 = do 
  h <- openFile "jabberwocky.txt" ReadMode

  contents  <- hGetContents h -- (lazy) stream
  putStrLn (take 10 contents) -- lazily fetch 10 chars

  hClose h

-- ------------------------------------------------------

main4 = do -- line stream
  h <- (openFile "jabberwocky.txt" ReadMode) 

  lines' <- lineStream h
  sequence_ (map putStrLn lines')

  hClose h
  where 
    lineStream h' = hGetContents h' >>= return . lines

-- ------------------------------------------------------
-- line stream using mapM_
main5 = do 
  h <- (openFile "jabberwocky.txt" ReadMode) 

  lines' <- lineStream h
  mapM_ putStrLn lines'

  hClose h
  where 
      lineStream h' = hGetContents h' >>= return . lines

-- ------------------------------------------------------
-- "trailing lambda" with forM_
main6 = do 
  h <- (openFile "jabberwocky.txt" ReadMode) 

  lines' <- lineStream h
  forM_ lines' $ \line -> do
                    let reversed = reverse line
                    putStrLn reversed

  hClose h
  where 
      lineStream h' = hGetContents h' >>= return . lines


-- ------------------------------------------------------
data Chunk = Chunk   {chunk :: String} 
           | LineEnd {chunk :: String, remainder :: String}
  deriving (Show)

parseChunk chunk
 = if rightS == B8.pack ""
     then Chunk   (toS leftS)
     else LineEnd (toS leftS) 
                  ((toS . B8.tail) rightS)
 where 
    (leftS, rightS) = B8.break (== '\n') chunk
    toS = map (chr . fromEnum) . B.unpack

main7 = do
  print $ parseChunk (B8.pack "gimble in the wabe:\nAll")
  -- LineEnd {chunk = "gimble in the wabe:", remainder = "All"}

-- ------------------------------------------------------

chunkStream :: Handle -> IO [L8.ByteString]
chunkStream h
  = do
      isEof <- hIsEOF h
      if isEof 
        then return []
        else do 
          chunk <- LB.hGet h 8
          rest  <- (chunkStream h)
          return (chunk:rest)

main8 = do
  h <- (openFile "jabberwocky.txt" ReadMode) 
  chunks <- chunkStream h
  print $ take 10 chunks

-- ------------------------------------------------------

-- lazy parseChunk
parseChunkL chunk 
 = if rightS == L8.pack ""
     then Chunk   (toS leftS)
     else LineEnd (toS leftS) 
                  ((toS . L8.tail) rightS)
 where 
    (leftS, rightS) = L8.break (== '\n') chunk
    toS = map (chr . fromEnum) . LB.unpack

processChunk' :: String -> [L8.ByteString] -> IO ()

processChunk' acc []
     = putStrLn acc -- terminate recursion

processChunk' acc (chunk:chunks)
  = case (parseChunkL chunk) of
      (Chunk chunk')
          -> processChunk' (acc ++ chunk') chunks
      (LineEnd chunk' remainder)
          -> do
              let line = acc ++ chunk'
              putStrLn line -- do something with line
              processChunk' remainder chunks

processChunk = processChunk' ""

main9 = do
  h <- openFile "jabberwocky.txt" ReadMode 
  chunkStream h >>= processChunk
  hClose h

-- ------------------------------------------------------
lineStream accChunks [] = [accChunks]
lineStream accChunks (chunk:chunks)
  = case (parseChunkL chunk) of
      (Chunk chunk')
          -> lineStream (accChunks ++ chunk') chunks
      (LineEnd chunk' remainder)
          -> (accChunks ++ chunk') : (lineStream remainder chunks)

toLines = lineStream ""

main10 = do
        h <- openFile "jabberwocky.txt" ReadMode

        lines' <- liftM toLines (chunkStream h)
        mapM_ putStrLn lines'

        hClose h

main11 = do
        h <- openFile "jabberwocky.txt" ReadMode

        chunks <- (chunkStream h)
        let lines' = toLines chunks
        -- vs 
        -- lines' <- liftM toLines (chunkStream h)

        mapM_ putStrLn lines'

        hClose h

-- ------------------------------------------------------
-- The Problems with Lazy IO

main12 = do
  h <- openFile "jabberwocky.txt" ReadMode

  firstLine <- hGetLine h    -- returns a string 
  contents <- hGetContents h -- returns a "promise"

  hClose h -- close file print $ words firstLine

  print $ words firstLine
  -- ["'Twas","brillig,","and","the","slithy","toves"]
  
  -- print $ words contents
  -- *** Exception: jabberwocky.txt: hGetContents: illegal operation (delayed read on closed handle)

-- ------------------------------------------------------
-- Resource management with bracket

main13 = do
  h <- (openFile "jabberwocky.txt" ReadMode) 
  useResource h
  hClose h
  where
    useResource h' = (stream h') >>= mapM_ putStrLn
    stream h' = hGetContents h' >>= return . lines

-- ------------------------------------------------------

main14 = do
  contents <- readFile "jabberwocky.txt" 
  mapM_ putStrLn (lines contents)

-- ------------------------------------------------------

main15 = do
  withFile "jabberwocky.txt" ReadMode enumerateLines 
  where
       enumerateLines h = lines' h >>= mapM_ putStrLn
       lines' h' = hGetContents h' >>= return . lines

-- ------------------------------------------------------

{- 
bracket
     (openFile "filename" ReadMode) -- acquire resource
     hClose                         -- release resource
     (\h -> "do some work")

-- where

bracket ::  IO a               -- before action
               -> (a -> IO b)  -- after action
               -> (a -> IO c)  -- do action
               -> IO c         -- result

finally :: IO a       -- some action
             -> IO b  -- final action: runs afterwards
             -> IO b  -- result
-}
