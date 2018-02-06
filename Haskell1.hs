module Opdracht1
    where
    
 -- opdracht 1 Geofig is het data type en bijv Vierkant is de Value Constructor
    data Geofig = Vierkant 
                  {
                     zijde  :: Double,
                     kleur  :: Kleur
                  }|
                  Driehoek
                  {
                     zijde  :: Double,
                     kleur  :: Kleur
                  }|
                  Circel
                  {
                      straal :: Double,
                      kleur  :: Kleur
                  }|
                  Rechthoek
                  {
                      zijdeL :: Double,
                      zijdeB :: Double,
                      kleur  :: Kleur
                  }
                  deriving (Show,Eq) --deriving geeft aan wat je met de data kan doen Eq is bijv de 'permissie' dat deze data types vergeleken kunnen worden


---------------------------------------------------------------------
    data Kleur = Rood | Blauw | Geel | Oranje deriving (Show,Eq)
---------------------------------------------------------------------
    -- geometrische objecten 
    vierkant  = Vierkant  3.0 Blauw 
    driehoek  = Driehoek  5.0 Rood 
    circel    = Circel    1.5 Geel 
    rechthoek = Rechthoek 4.65  5.21 Oranje 
---------------------------------------------------------------------
    --opdracht 3
    oppervlakte :: Geofig->Double
    oppervlakte x
                |isVierkant  x == True = 2*(zijde x)
                |isCircel    x == True = pi*((straal x)^2)
                |isDriehoek  x == True = ((zijde x)*(zijde x))/2
                |isRechthoek x == True = (zijdeB x)*(zijdeL x)
                        where pi=3.14159265359  


---------------------------------------------------------------------
    --opdracht 4
    omtrek :: Geofig->Double
    omtrek x
                |isVierkant  x == True = 4*(zijde x)
                |isCircel    x == True = 2*pi*(straal x)
                |isDriehoek  x == True = (zijde x)*3
                |isRechthoek x == True = 2*((zijdeB x)+(zijdeL x))
                        where pi=3.14159265359


--------------------------------------------
    --hulp functies 
    isVierkant  (Vierkant  _ _)   = True
    isVierkant  (Driehoek  _ _  ) = False
    isVierkant  (Circel    _ _  ) = False
    isVierkant  (Rechthoek _ _ _) = False
--------------------------------------------
    isDriehoek  (Driehoek  _ _)   = True
    isDriehoek  (Vierkant  _ _  ) = False
    isDriehoek  (Circel    _ _  ) = False
    isDriehoek  (Rechthoek _ _ _) = False 
--------------------------------------------
    isCircel    (Circel    _ _)   = True
    isCircel    (Vierkant  _ _  ) = False
    isCircel    (Driehoek  _ _  ) = False
    isCircel    (Rechthoek _ _ _) = False
--------------------------------------------
    isRechthoek (Rechthoek _ _ _) = True
    isRechthoek (Vierkant  _ _  ) = False
    isRechthoek (Driehoek  _ _  ) = False
    isRechthoek (Circel    _ _  ) = False

