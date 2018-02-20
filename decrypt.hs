module Decrypt
    where

        import System.IO

        readFromFile = do
            bestand <- openFile "bestand.txt" ReadMode
            content <- hGetContents bestand
            putStr content
            hClose bestand
            

                           