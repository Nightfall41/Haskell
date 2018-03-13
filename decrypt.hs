module Decrypt
    where

        import System.IO
        import System.Random
        import Control.Monad
        import Data.Function
        import Data.Bits
        import Data.Char

        main = do
            cipher <- readFile "bestand.txt"
            key    <- readFile "sleutel.key"
            let decryptedFile = stringXor key cipher
            writeFile "bestand.txt" decryptedFile
            writeFile "sleutel.key" ("deze sleutel is verouderd")
            return(decryptedFile)


        stringXor :: String -> String -> String
        stringXor = zipWith (fmap chr . xor) `on` map ord

                           