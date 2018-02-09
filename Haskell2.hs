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
    
    data Box x = Leeg
                | Gevuld x
                deriving (Show)


    -- opdracht 3
    java   = Boek 20.00 "Learn Java" "Schaum"
    scrum  = Boek 4.99  "Scrum for dummies" "Dummies"
    haskell= Boek 0.0 "Learn you a great Haskell" "Wessel Oele"
    sql    = Boek 14.99 "Databases" "Some Asian Dude"
    google = Boek 60.00 "Google, everything you need" "internet"

    list  = [java,scrum,haskell,google,sql]

    isEqual :: Boek->Boek->Bool
    isEqual a b = a==b

    sortBoek :: [Boek]->[Boek]
    sortBoek = sortBy (comparing titel) 

    placeBoekInBox :: [Boek]->(Box [Boek])  
    placeBoekInBox x = Gevuld x 

    extractBoekFrombox :: (Box [Boek])->[Boek]
     


