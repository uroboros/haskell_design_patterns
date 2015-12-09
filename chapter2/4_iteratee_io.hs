import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr)

-- ------------------------------------------------------

data Chunk = Chunk   {chunk :: String} 
           | LineEnd {chunk :: String, remainder :: String}
  deriving (Show)

parseChunk chunk
 = if rightS == B8.pack ""
     then Chunk   (toS leftS)
     else LineEnd (toS leftS) ((toS . B8.tail) rightS)
 where 
    (leftS, rightS) = B8.break (== '\n') chunk

toS = map (chr . fromEnum) . B.unpack -- helper

-- ------------------------------------------------------

-- iterF :: String -> IterResult

-- This type represents to idea of "Iteratee"
newtype Iter = Iter {runIter :: B8.ByteString -> IterResult}

-- IterResult can indicate 2 possibilities:
data IterResult 
  = HaveLine {line :: String, residual :: String}
    -- a line has been accumulated (with possible residual)
  | NeedChunk Iter
  -- need more chunk data

instance Show IterResult where
  show (HaveLine line residual) = "HaveLine " ++ line ++ "|" ++ residual
  show (NeedChunk _) = "NeedChunk"


chunkIter :: Iter
chunkIter = Iter (go "")
    where 
      go :: String -> B8.ByteString -> IterResult 
      go acc chunk =
            case (parseChunk chunk) of
              (Chunk chunk')
                -> NeedChunk (Iter (go (acc ++ chunk')))
              (LineEnd chunk' residual')
                -> HaveLine (acc ++ chunk') residual'

-- ------------------------------------------------------

main1 = do 
    h <- openFile "jabberwocky.txt" ReadMode
    
    chunk1 <- B.hGet h 50 --
    print $ runIter chunkIter chunk1
    -- HaveLine "'Twas brillig, and the slithy toves"
    --          "Did gy"

-- With a chunk size smaller than the first file line,
-- we instead get an IterResult of "NeedChunk"
main2 = do 
    h <- openFile "jabberwocky.txt" ReadMode
    
    chunk1 <- B.hGet h 25
    print $ runIter chunkIter chunk1
    -- NeedChunk

-- ------------------------------------------------------

main3 = do 
    h <- openFile "jabberwocky.txt" ReadMode
    
    chunk1 <- B.hGet h 25
    let (NeedChunk iter1) = runIter chunkIter chunk1

    chunk2 <- B.hGet h 25
    let (HaveLine line residual) = runIter iter1 chunk2
    putStrLn line
    -- "'Twas brillig, and the slithy toves"

-- ------------------------------------------------------
type Enumerator = Iter -> IO IterResult

enumerateFile :: FilePath -> Enumerator
enumerateFile path initIter =
    withFile path ReadMode $ \h ->
      let 
        go iter = do
          isEOF <- hIsEOF h
          if isEOF
              then return (HaveLine "End Of File" "") -- cheat!
              else do -- feed next chunk to iteratee
                chunk <- B.hGet h 8
                check $ runIter iter chunk
        check (NeedChunk iterNext)      = go iterNext
        check (HaveLine line residual)  = do
                              putStrLn line
                              check $ runIter initIter (B8.pack residual)
        in go initIter

main4 = enumerateFile "jabberwocky.txt" chunkIter
