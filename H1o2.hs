
module Rsa
    where
        import Data.Char
        -- opdracht 1a
        euclid :: Integer-> Integer -> Integer
        euclid a 0 = abs a
        euclid a b =abs (euclid b (mod a b))

        -- opdracht 1b  HOOFDPIJN
        egcd :: Integer -> Integer -> (Integer,Integer,Integer)
        egcd 0 b = (b, 0, 1)
        egcd a b =
            let (g, s, t) = egcd (b `mod` a) a
            in (g, t - (b `div` a)*s, s)

        mijnEgcd::Integer -> Integer -> Integer
        mijnEgcd a b
            | negTesta (egcd a b) < 0 = negTesta (egcd a b) + (a `mod` b)
            | negTesta (egcd a b) >= 0 = negTesta (egcd a b)
                    where negTesta (_,a,_) = a
---------------------------------------------------------------------------------------------------------------
        -- De waardes van de  EncryptionKey,  DecryptionKey,  Modulus en Phi voor alice
        modulusAlice=101*499
        modAccentAlice=(101-1)*(499-1)
        eAlice = 103
        dAlice = mijnEgcd eAlice modAccentAlice
        -- De waardes van de  EncryptionKey,  DecryptionKey,  Modulus en Phi voor Bob
        modulusBob=337*479
        modAccentBob=(337-1)*(479-1)
        eBob=23
        dBob= mijnEgcd eBob modAccentBob
---------------------------------------------------------------------------------------------------------------
        -- Encryptie berekening
        aliceEncrypt:: Integer->Integer
        aliceEncrypt  x =
            x^eAlice `mod`modulusAlice
        -- Decryptie berekening
        aliceDecrypt:: Integer->Integer
        aliceDecrypt  x =
            x^dAlice `mod`modulusAlice
---------------------------------------------------------------------------------------------------------------
        bobEncrypt  :: Integer->Integer
        bobEncrypt    x =
            x^eBob  `mod` modulusBob

        bobDecrypt  :: Integer->Integer
        bobDecrypt    x =
            x^dBob  `mod` modulusBob
---------------------------------------------------------------------------------------------------------------
        asciiEncryptA :: Char->Integer
        asciiEncryptA x =
            let cipher = ord x
            in aliceEncrypt (toInteger cipher)

        asciiEncryptB :: Char->Integer
        asciiEncryptB x =
            let cipher = ord x
            in bobEncrypt (toInteger cipher)
---------------------------------------------------------------------------------------------------------------
        asciiDecryptA :: Integer->Char
        asciiDecryptA x =
            let msg = aliceDecrypt x
            in chr (fromInteger msg)

        asciiDecryptB :: Integer->Char
        asciiDecryptB x =
            let msg = bobDecrypt x
            in chr (fromInteger msg)
---------------------------------------------------------------------------------------------------------------
        -- De privesleutel moet geheim blijven de volgorde is als volgt:
        -- iedereen met de publieke sleutel kan versleutelen
        -- iedereen met de prive sleutel kan ontsleutelen
        -- door dit uit te voeren is het bijna onmogelijk om met de publieke sleutel te ontsleutelen
        -- versleutel met dAlice->versleutel met eBob->ontsleutel met dBob -> ontsleutel eAlice

        encryptedComs :: Integer->Integer
        encryptedComs x =
            let cipher        =aliceDecrypt x   -- prive sleutel encryptie van alice
                cipherSquared =bobEncrypt cipher -- publieke sleutel encryptie van bob
            in  aliceEncrypt(bobDecrypt cipherSquared) --ontsleuteling met bobs prive sleutel en dan ontsleutling van alice's publieke sleuteling.
