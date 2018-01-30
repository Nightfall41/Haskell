module Opdracht1
    where

    type Lengte  = Double
    type Breedte = Double
    type Straal  = Double

    data Geofig = Vierkant | Driehoek | Circel | Rechthoek

    data Colour = RGB Int Int Int

    RGB :: Int -> Int -> Int -> Colour

    -- Bro https://wiki.haskell.org/Colour
    -- deze ook goeie https://stackoverflow.com/questions/18204308/haskell-type-vs-data-constructor

