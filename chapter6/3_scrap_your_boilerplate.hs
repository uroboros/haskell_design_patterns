{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}

import Data.Typeable

-- ------------------------------------------------------
-- Generic traversal

data Book     = Book    Title [Chapter]
  deriving(Show, Typeable)
data Chapter  = Chapter Title [Section]
  deriving(Show, Typeable)
data Section  = Section Title [Para]
  deriving(Show, Typeable)

type Title = String; type Para = String

haskellDP = Book "Haskell Design Patterns" chapters

chapters  = [Chapter "The building blocks" sections1,
             Chapter "IO Patterns" sections2]

sections1 = [Section "1.1" ["Lorem lorem"],
             Section "1.2" ["Lorem lorem", "Lorem lorem"]]

sections2 = [Section "2.1" ["Ipsum Ipsum"],
             Section "2.2" ["Ipsum Ipsum", "Ipsum Ipsum"]]

fSection (Section t lines') = Section "!!!" lines'

aSection = Section "1.1" ["Lorem", "lorem"]
main1 = print $ (fSection aSection)
        -- Section "!!!" ["Lorem","lorem"]

-- We want to apply this function to all Sections embedded in a Book.

-- ------------------------------------------------------
-- Generic traversal: Typesafe cast with TYPEABLE

{-
  {- LANGUAGE DeriveDataTypeable -}
  import Data.Typeable

  data Book = ...
    deriving(Show, Typeable)
  data Chapter  = ...
    deriving(Show, Typeable)
  data Section  = ...
    deriving(Show, Typeable)
-}

-- Let's get to know Typeable.cast:

-- cast 'a' :: Maybe Char
--  -> Just 'a'
-- cast 'a' :: Maybe Int
--  -> Nothing


-- (cast (Section "title" ["line1", "line2"])) :: Maybe Section
--   -> (Just (Section "title" ["line1", "line2"]))
-- (cast (Book "title" [])) :: Maybe Section
--   -> Nothing

-- cast (++ "a") :: Maybe String
--  -> Nothing 
-- cast (++ "a") :: Maybe (String -> String)
--  -> Just (++ "a")

-- ------------------------------------------------------
-- Generic traversal: Typesafe function application

typesafeF :: (Typeable a, Typeable b) => (b -> b) -> a -> a
typesafeF f 
  = case (cast f) of
      Just f' -> f'
      Nothing -> id

-- e.g.

-- typesafeF (+1) 3 
--  -> 4
-- typesafeF (+1) "3" 
--  -> "3"

main2 = do
  -- apply fSection
  print $ (typesafeF fSection) (Section "1.1" ["Lorem lorem"])
  -- Section "!!!" ["Lorem lorem"]

  -- apply "id" when type does not match
  print $ (typesafeF fSection) (Book "title" [])
  -- Book "title" []

-- ------------------------------------------------------
-- Shallow Traversal and the Data typeclass

{- 
    gmap f (Chapter title sections) 
      = Chapter (f title) (f sections)
    -- INVALID
-}

-- we need "Rank2Types"

-- Data' inherits from Typeable
class Typeable a => Data' a where
  gmap :: (forall b. Data' b => b -> b) 
            -> a -> a
-- (defined in Data.Data)

instance Data' Book where
  gmap f (Book title chapters) 
    = Book (f title) (f chapters)

instance Data' Chapter where
  gmap f (Chapter title sections) 
    = Chapter (f title) (f sections)

instance Data' Section where
  gmap f (Section title paras) 
    = Section (f title) (f paras)

instance Data' a => Data' [a] where
  gmap f []     = []
  gmap f (x:xs) = f x : f xs

instance Data' Char where
  gmap f c = c

main3 = do
  print $ gmap (typesafeF fSection) chapter
  {- 
    Chapter "The building blocks" 
            [Section "1.1" ["Lorem lorem"],
             Section "1.2" ["Lorem lorem","Lorem lorem"]]
    
    ... the sections are not reached by the traversal
  -}

  print $ gmap (typesafeF fSection) sections1
  {- 
    [Section "!!!" ["Lorem lorem"],
     Section "1.2" ["Lorem lorem","Lorem lorem"]]

    ... only the first Section is affected by fSection
  -}

  where chapter = (Chapter "The building blocks" sections1)

traverseBottomUp :: Data' a =>(forall b . Data' b => b->b) -> a -> a
traverseBottomUp f x = f (gmap (traverseBottomUp f) x)

traverseTopDown :: Data' a =>(forall b . Data' b => b->b) -> a -> a
traverseTopDown f x = gmap (traverseTopDown f) (f x)

main4 = do
  -- this time our traversal is reaching all Sections...
  print $ traverseBottomUp (typesafeF fSection) chapter
  {-
    Chapter "The building blocks" 
            [Section "!!!" ["Lorem lorem"],
            Section "!!!" ["Lorem lorem","Lorem lorem"]]
  -}
  print $ traverseBottomUp (typesafeF fSection) sections1
  {-
    [Section "!!!" ["Lorem lorem"],
     Section "!!!" ["Lorem lorem","Lorem lorem"]]
  -}
  print $ traverseTopDown  (typesafeF fBook)    haskellDP
  {-
    Book "!!!" [Chapter "The building blocks" 
                        [Section "1.1" ["Lorem lorem"],
                         Section "1.2" ["Lorem lorem","Lorem lorem"]],
                Chapter "IO Patterns" 
                        [Section "2.1" ["Ipsum Ipsum"],
                         Section "2.2" ["Ipsum Ipsum","Ipsum Ipsum"]]]
  -}
  where 
    chapter = (Chapter "The building blocks" sections1)
    fBook (Book t chapters) = Book "!!!" chapters
