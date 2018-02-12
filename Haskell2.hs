module Opdracht2
    where
    
    import Data.List
    import Data.Ord

    -- opdracht 1&2
    data Boek = Boek
                {
                    prijs  :: Float,
                    titel  :: String,
                    auteur :: String
                } deriving (Show,Eq,Ord)

----------------------------------------------------------------------------
    --opdracht 4 en 6
    data Box x = GeenInhoud | Gevuld x
                deriving Show

    data Zak x = Leeg | Vol x 
                deriving Show

    data List x = Hol | Hoofd x (List x)
                deriving (Show,Eq,Ord)

    -- instantie van de Functor, voor Box ,Zak en List 
    instance Functor Box where  
            fmap f (Gevuld x) = Gevuld (f x)
            fmap f GeenInhoud = GeenInhoud
    
    instance Functor Zak where
            fmap f (Vol x)    = Vol (f x)
            fmap f Leeg       = Leeg
    instance Functor List where 
            fmap f (Hoofd x (Hoofd x)) = Hoofd (f x) Hoofd(f x)
            fmap f Hol               = Hol     
            fmap f Hoofd x (Hol)     = Hoofd (f x)  

----------------------------------------------------------------------------            
            
    -- opdracht 3
    java   = Boek 20.00 "Learn Java" "Schaum"
    scrum  = Boek 4.99  "Scrum for dummies" "Dummies"
    haskell= Boek 0.0 "Learn you a great Haskell" "Wessel Oele"
    sql    = Boek 14.99 "Databases" "Some Asian Dude"
    google = Boek 60.00 "Google, everything you need" "internet"

    list       = [java,scrum,haskell,google,sql]
    
    isEqual :: Boek->Boek->Bool
    isEqual a b = a==b
    
    sortBoek :: [Boek]->[Boek]
    sortBoek = sortBy (comparing titel) 
----------------------------------------------------------------------------
    --opdracht 5
    placeBoekInBox :: [Boek]->(Box [Boek])  
    placeBoekInBox x = Gevuld x 

    extractBoekFrombox :: (Box [Boek])->[Boek]
    extractBoekFrombox x = insideBox x 
----------------------------------------------------------------------------
    -- opdracht 8
    javaZak    = Vol java
    scrumZak   = Vol scrum
    haskellZak = Vol haskell
    sqlZak     = Vol sql
    googleZak  = Vol google

    --in deze functie kan je alles stoppen en dat alles gaat in een box
    toBox :: (x)->(Box (x)) 
    toBox x = Gevuld x
    -- gebruik placeInBox methode in de GHCI
    placeInBox x = fmap toBox x
    
---------------------------------------
--Hulpfunctie
    insideBox (Gevuld x)=x

    

