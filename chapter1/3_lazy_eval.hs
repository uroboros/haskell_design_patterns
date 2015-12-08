import System.Random

-- Lazy Eval ------------------------------------------------------

doomedList = [2, 3, 5, 7, undefined]

-- (Prelude.take)
-- take 0 xs = []
-- take n (x:xs) = x : (take (n-1) xs)

main1 = print $ take 4 doomedList
       -- [2,3,5,7]


-- Streams --------------------------------------------------------

infinite42s = 42 : infinite42s
main2 = print $ take 5 infinite42s
       -- [42,42,42,42,42]


generate :: StdGen -> (Int, StdGen)
generate g = random g :: (Int, StdGen)

-- import System.Random
main3 = do
  gen0 <- getStdGen 
  let (int1, gen1) = (generate gen0)
  let (int2, gen2) = (generate gen1)
  return (int1, int2)


randInts' g = (randInt, g) : (randInts' nextGen)
          where (randInt, nextGen) = (generate g)

randInts g = map fst (randInts' g)
randAmounts g = map (\x -> x `mod` 100) (randInts g)

main4 = do
     g <- getStdGen
     print $ take 3 (randInts g)
     print $ take 3 (randAmounts g)


-- Modeling change with streams -----------------------------------

bankAccount openingB (amt:amts)
     = openingB : bankAccount (openingB + amt) amts
   
main5 = do 
    print $ take 4 (bankAccount 0 [-100, 50, 50, 1])
    -- [0,-100,-50,0]

    g <- getStdGen
    print $ (take 4 (bankAccount 0 (randAmounts g)))

