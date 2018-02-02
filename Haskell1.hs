module Opdracht1
    where
    
        
    

    data Geofig =   Vierkant Double Kleur
                    | Driehoek Double Kleur
                    | Circel Double Kleur
                    | Rechthoek Double  Double Kleur
                    deriving Show


    data Kleur = Rood | Blauw | Geel | Zwart | Oranje deriving Show

