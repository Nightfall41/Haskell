module MansNotHot
    where
-- pattern matching ... faculteit is een een recursive formule dus je moet constant de functie oproepen door te zeggen wanneer er 0 binne komt 1 te geven breek je de recursie
faca:: Integer->Integer
faca 0=1
faca x = x*faca(x-1)
-- deze functie gebruikt guards basicly het zijn ifjes if dit dan dat zelfde als functie FACA
facb :: Integer -> Integer
facb x
    | x == 0 = 1
    | otherwise =x*facb(x-1)
-- berekent nulpunten maakt gebruik van guards
nulpuntena :: Double->Double->Double->[Double]
nulpuntena a b c
    |sqrt(b^2-(4*a*c))==0 = [((-b))/(2*a)]
    |sqrt(b^2-(4*a*c))>0 = [((-b)-sqrt(b^2-(4*a*c)))/(2*a),((-b)+sqrt(b^2-(4*a*c)))/(2*a)]

-- berekent nulpunten maakt gebruikt met guards en de where keywoord (variable naam)
nulpuntenb :: Double->Double->Double->[Double]
nulpuntenb a b c
    |sqrt(disc)==0 = [((-b)+sqrt(disc))/(2*a)]
    |sqrt(disc)>1 = [((-b)-sqrt(disc))/(2*a),((-b)+sqrt(disc))/(2*a)]
        where disc = b^2-(4*a*c)   -- where is de variable naam dus in dit geval disc staat gelijk aan de discriminant


-- je maakt een list met tuples in 1 tuple zit 3 elementen, die elementen bestaan onder de voorwaarde | dat ze bestaan uit een verzameling van 6 elementen en dat de veelvoud 5  van de elementen opgeteld 0 is (modulus)
dobbel = [(dice_1,dice_2,dice_3)|dice_1<-[1..6],dice_2<-[1..6],dice_3<-[1..6], (dice_1+dice_2+dice_3) `mod` 5 == 0]


-- same here but inplaats van veelvoud 5 kan je zelf een getal meegeven waarvan de veelvoud wordt gegeven
dobbelV2 :: Int->[(Int,Int,Int)]
dobbelV2 x=[(dice_1,dice_2,dice_3)|dice_1<-[1..6],dice_2<-[1..6],dice_3<-[1..6], (dice_1+dice_2+dice_3) `mod` x == 0]
