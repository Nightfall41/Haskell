module Opdracht2
    where

    import Data.List

    -- opdracht 1&2
    data Boek = Boek
                {
                    prijs  :: Float,
                    titel  :: String,
                    auteur :: String
                } deriving (Show,Eq,Ord)

    -- opdracht 3
    java   = Boek 20.00 "Learn Java" "Schaum"
    scrum  = Boek 4.99  "Scrum for dummies" "Dummies"
    haskell= Boek 0.0 "Learn you a great Haskell" "Wessel Oele"
    sql    = Boek 14.99 "Databases" "Some Asian Dude"
    google = Boek 60.00 "Google, everything you need" "internet"

    lijst  = [java,scrum,haskell,sql,google]

    isEqual :: Boek->Boek->Bool
    isEqual a b = a==b

    sortBoek :: [Boek]->[Boek]
    sortBoek [] =[]
    --sortBoek (x:xs) = sort 