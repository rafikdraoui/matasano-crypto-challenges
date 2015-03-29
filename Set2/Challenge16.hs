module Set2.Challenge16 where

import Control.Applicative ((<$>))

import Control.Monad.Random (evalRandIO)

import Set1.Challenge2 (xor')

import Set2.Challenge9 (pkcs7Padding)
import Set2.Challenge10 (encryptCBC, decryptCBC)
import Set2.Challenge11 (generateRandomBytes)
import Set2.Challenge15 (stripPadding)

import Utils (replace, splitOn)


{- ENCRYPT -}

-- URL-quote escaping for characters '=' and ';'
escape :: String -> String
escape = replace '=' "%3D" . replace ';' "%3B"

encode :: String -> String
encode s = "comment1=cooking%20MCs;userdata=" ++ escape s ++
           ";comment2=%20like%20a%20pound%20of%20bacon"

encrypt :: String -> String -> String
encrypt key = encryptCBC key . pkcs7Padding 16 . encode



{- DECRYPT -}

isAdmin :: String -> Bool
isAdmin s = ["admin", "true"] `elem` pairs
    where pairs = splitOn '=' <$> splitOn ';' s

decrypt :: String -> String -> Either String String
decrypt key = stripPadding . decryptCBC key

isValid :: String -> String -> Either String Bool
isValid key s = isAdmin <$> decrypt key s



{- BREAK
 -
 - The "comment1=..." prefix is exactly two blocks long, so the user data will
 - always start at the beginning of the third block. If we invert all the bytes
 - (i.e. xor with all 1s) of the second block of ciphertext, then the second
 - block of plaintext will be totally scrambled, and the third block of
 - plaintext will be equal to the inverted value of the user data.
 -
 - Since we want the block ";admin=true;...." to be present in the plaintext,
 - we can use the inverted value of this target block as our user input, and
 - after tampering with the ciphertext, we will get the inverse of this in the
 - plaintext, that is, we'll get our target block.
 -}

targetBlock :: String
targetBlock = ";admin=true;\0\0\0\0"

invert :: String -> String
invert s = s `xor'` replicate (length s) '\1'

getCiphertext :: String -> String
getCiphertext key =
    let
        enc = encrypt key $ invert targetBlock
        secondBlock = take 16 $ drop 16 enc
    in
        take 16 enc ++ invert secondBlock ++ drop 32 enc

challenge16 :: IO (Either String Bool)
challenge16 = do
    key <- evalRandIO $ generateRandomBytes 16
    let ciphertext = getCiphertext key
    return $ isValid key ciphertext
