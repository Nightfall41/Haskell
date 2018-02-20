
module Encrypt
        where

            import System.IO
            import Data.Char



            readFromFile = do
                bestand <- openFile "bestand.txt" ReadMode
                content <- hGetContents bestand
                fmap Just content
                hClose bestand
            
            