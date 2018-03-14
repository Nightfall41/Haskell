module Decrypt
    where

        import System.IO
        import System.Random
        import Control.Monad
        import Data.Function
        import Data.Bits
        import Data.Char

        main = do
            cipher <- readFile "cipher.txt"
            key    <- readFile "sleutel.key"
            let decryptedFile = stringXor key cipher
            writeFile "decryptedContent.txt" decryptedFile
            return(decryptedFile)

        stringXor :: String -> String -> String
        stringXor = zipWith (fmap chr . xor) `on` map ord

                           