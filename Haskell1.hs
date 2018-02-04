module Opdracht1
    where
    
        
    

    data Geofig =   Vierkant Double Kleur String
                    | Driehoek Double Kleur String
                    | Circel Double Kleur String
                    | Rechthoek Double  Double Kleur String
                    deriving (Show,Eq)


    data Kleur = Rood | Blauw | Geel | Oranje deriving (Show,Eq)

    vierkant  = Vierkant  3.0 Blauw "vierkant"
    driehoek  = Driehoek  5.0 Rood "driehoek"
    circel    = Circel    1.5 Geel "circel" 
    rechthoek = Rechthoek 4.65  5.21 Oranje "rechthoek"

    oppervlakte :: Geofig->Double
    oppervlakte x 
                | (Geofig x  _ _ soort) == "vierkant" = zijde*zijde
                    where zijde= (Geofig x  zijde _ _)
                          soort= (Geofig x _ _ soort)




    

