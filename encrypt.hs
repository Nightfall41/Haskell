
module Encrypt
        where

            import System.IO
            import System.Random
            import Control.Monad
            import Data.Function
            import Data.Bits
            import Data.Char


            main = do
                content <- readFile "bestand.txt"
                gen <- newStdGen
                let randomKeyGen = take (length content) (randomRs ('a','z') gen)
                let cipher = stringXor content randomKeyGen
                writeFile "sleutel.key" randomKeyGen
                writeFile "cipher.txt" cipher
                return(cipher)


            stringXor :: String -> String -> String
            stringXor = zipWith (fmap chr . xor) `on` map ord

            --adit.io monad in pictures
            --do learn you a haskell
            

