import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr)

main1 = do 
        h <- openFile "jabberwocky.txt" ReadMode
        loop h
        hClose h
        where loop h' = do  
              isEof <- hIsEOF h'
              if isEof 
                then putStrLn "DONE..."
                else do 
                      line  <- hGetLine h' -- *** GET LINE
                      print $ words line
                      loop h'

main2 = do 
        h <- openFile "jabberwocky.txt" ReadMode
        loop h
        hClose h
       where loop h' = do  
              isEof <- hIsEOF h'
              if isEof
                then putStrLn "DONE..."
                else do 
                      chunk  <- B.hGet h' 8 -- *** GET CHUNK
                      print . words . show $ chunk
                      loop h'

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

main3 = do
  print $ parseChunk (B8.pack "AAA\nBB")
  -- LineEnd {chunk = "AAA", remainder = "BB"} 
  print $ (parseChunk (B8.pack "CCC"))
  -- Chunk {chunk = "CCC"}

main4 = do 
        fileH <- openFile "jabberwocky.txt" ReadMode
        loop "" fileH
        hClose fileH
       where loop acc h = do  
              isEof <- hIsEOF h
              if isEof 
                then do putStrLn acc; putStrLn "DONE..."
                else do 
                      chunk <- B.hGet h 8
                      case (parseChunk chunk)  of
                        (Chunk chunk')
                            -> do
                                let accLine = acc ++ chunk'
                                loop accLine h
                        (LineEnd chunk' remainder)
                            -> do
                                let line = acc ++ chunk'
                                putStrLn line -- do something with line
                                loop remainder h
                      return ()
