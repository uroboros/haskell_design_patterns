-- Algebraic types and pattern matching ---------------------------

-- Type Combination ("product of types")
type Name = String
type Age = Int
data Person = P String Int 


-- Type Alternatives ("sum of types")
data MaybeInt = NoInt | JustInt Int
maybeInts = [JustInt 2, JustInt 3, JustInt 5, NoInt]


-- "sum of products"
data Maybe' a = Nothing' | Just' a
  deriving Show

fMaybe f (Just' x) = Just' (f x)
fMaybe f Nothing' = Nothing'

fMaybes = map (fMaybe (* 2)) [Just' 2, Just' 3, Nothing']
-- [Just' 4, Just' 6, Nothing']


-- Recursive types ------------------------------------------------

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show

size :: Tree a -> Int
size (Leaf x) = 1
size (Branch t u) = size t + size u + 1

main1 = print $ size aTree -- 5
  where aTree = Branch (Leaf 3)
                       (Branch (Leaf 5) (Leaf 7))


-- Parametric polymorphism ----------------------------------------

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length xs

main2 = do
  print $ length' [1,2,3,5,7]
  -- 5
  print $ length' ['1','2','3','5','7']
  -- 5

-- Ad-hoc polymorphism (overloading) ------------------------------

{- sketch code (intPlus and floatPlus not defined)

class Num a where
  (+) :: a -> a -> a

instance Int Num where
  (+) :: Int -> Int -> Int
  x + y = intPlus x y

instance Float Num where
  (+) :: Float -> Float -> Float
  x + y = floatPlus x y

-}

-- Ad-hoc polymorphism: Alternation based --------------------------

data Shape = Circle Float | Rect Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect length width) = length * width

-- dispatch on algebraic type constructor
main3 = do
  print $ area (Circle 1)
  -- 3.1415927

  print $ area (Rect 11 12)
  -- 132.0

-- Ad-hoc polymorphism: Class based --------------------------

data Circle2 = Circle2 Float
data Rect2 = Rect2 Float Float

class Shape2 a where
  area2 :: a -> Float

instance Shape2 Circle2 where
  area2 (Circle2 r) = pi * r^2

instance Shape2 Rect2 where
  area2 (Rect2 length' width') = length' * width'

-- dispatch on type-class implementation
main4 = do
  print $ area2 (Circle2 1)
  -- 3.1415927

  print $ area2 (Rect2 11 12)
  -- 132.0


-- Polymorphic dispatch and the visitor pattern --------------

data CustomerEvent = InvoicePaid Float | InvoiceNonPayment
data Customer = Individual Int | Organisation Int

payment_handler :: CustomerEvent -> Customer -> String
-- 4 permutations of CustomerEvent-Customer
payment_handler (InvoicePaid amt) (Individual custId)
  = "SendReceipt for " ++ (show amt)
payment_handler (InvoicePaid amt) (Organisation custId)
  = "SendReceipt for " ++ (show amt)
payment_handler InvoiceNonPayment (Individual custId)
  = "CancelService for " ++ (show custId)
payment_handler InvoiceNonPayment (Organisation custId)
  = "SendWarning for " ++ (show custId)

main5 = do
  print $ payment_handler (InvoicePaid 112.2) (Individual 1)
  print $ payment_handler (InvoicePaid 112.2) (Organisation 11)
  -- "SendReceipt for 112.2"
  print $ payment_handler InvoiceNonPayment (Individual 1)
  -- "CancelService for 1"
  print $ payment_handler InvoiceNonPayment (Organisation 11)
  -- "SendWarning for 11"

