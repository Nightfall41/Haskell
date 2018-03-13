module Voorbeeld where

import Control.Applicative
import Data.Char
import Data.List
import System.Info
       
data Box x = Leeg| Box x deriving Show

instance  Eq x=>Eq( Box x) where
  Box a == Box b = (a==b)
  Leeg==Leeg = True
  _==_=False

instance Functor Box where
  fmap f Leeg = Leeg
  fmap f (Box x) =Box (f x)

instance Applicative Box where
  pure x = Box x
  Leeg <*> _ = Leeg
  (Box x) <*> iets = x <$> iets

instance Monad Box where
  return x = Box x
  Box x >>= f = f x


b1 = Box 5.0

--monadische versie van normale functies:
kwadraat x = return (x*x)

wortel x = return (sqrt x)

--een voorbeeld met de bind (>>=) operator
--Het gaat erom dat je kettinkjes kunt maken:
-- iets>>=volgende>>=volgende>>=volgende>>=etc.

voorbeeld = b1>>=kwadraat>>=wortel>>=(\x->return (2*x+1))>>=wortel

--Hetzelfde voorbeeld met do notatie:

pijp x = do
  a <- x
  b <- kwadraat a
  c <- wortel b
  d <- Box (2*c+1)
  wortel d
  
--een laatste voorbeeld met io:

input = getLine >>=(\x->return (reverse x))>>= putStrLn
                




sys = do
    print os
    print arch
    print compilerName
    print compilerVersion
