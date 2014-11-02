module Set2.Challenge10 where

import Set1.Challenge2 (xor')
import qualified Set1.Challenge7
import Set1.Utils (b64decodeFile, splitInGroupsOf)

import Set2.Challenge9 (pkcs7Padding)

-- Reference:
-- https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Cipher-block_chaining_.28CBC.29


-- Versions of ECB encryption that pad the input to a block size of 16

paddedECB :: (String -> String -> String) -> String -> String -> String
paddedECB func key text = func key paddedText
    where paddedText = concatMap (pkcs7Padding 16) $ splitInGroupsOf 16 text

encryptECB :: String -> String -> String
encryptECB = paddedECB Set1.Challenge7.encryptECB

decryptECB :: String -> String -> String
decryptECB = paddedECB Set1.Challenge7.decryptECB



data CBCRoundOutcome = CBCRoundOutcome {
    outputBlock :: String,
    nextIV :: String
}


-- Single CBC encryption step.
-- Takes the key, the encrypted previous block and the current plaintext block
-- as arguments, and return the encrypted block and the value to be used for
-- XORing in the next round (in that case, this is the same value as the
-- encrypted output block).
cbcEncryptRound :: String -> String -> String -> CBCRoundOutcome
cbcEncryptRound key previous current =
    let
        encryptedBlock = encryptECB key $ xor' previous current
    in
        CBCRoundOutcome {outputBlock = encryptedBlock, nextIV = encryptedBlock}


-- Single CBC decryption step.
-- Takes the key, the previous ciphertext block and the current ciphertext
-- block as arguments, and return the decrypted block and the value to be used
-- for XORing in the next round (in that case, this is the current
-- ciphertext block).
cbcDecryptRound :: String -> String -> String -> CBCRoundOutcome
cbcDecryptRound key previous current =
    let
        decryptedBlock = xor' previous $ decryptECB key current
    in
        CBCRoundOutcome {outputBlock = decryptedBlock, nextIV = current}


-- Apply a function to each element of the list, passing an accumulating
-- parameter at each step and returning the resulting list.
-- This is essentially `Data.List.mapAccumL` modified to handle functions
-- with a CBCRoundOutcome return type.
mapAccum :: (String -> String -> CBCRoundOutcome) -> String -> [String] -> [String]
mapAccum _ _ [] =  []
mapAccum f acc (x:xs) = y:ys
    where outcome = f acc x
          y = outputBlock outcome
          ys = mapAccum f (nextIV outcome) xs


-- Run CBC {en,de}cryption on the given text according to the given CBC
-- round function.
runCBC :: (String -> String -> String -> CBCRoundOutcome)
       -> String -> String -> String
runCBC roundFunction key text =
    let blocks = splitInGroupsOf 16 text
        iv = replicate 16 '\0'
    in
        concat $ mapAccum (roundFunction key) iv blocks


encryptCBC :: String -> String -> String
encryptCBC = runCBC cbcEncryptRound

decryptCBC :: String -> String -> String
decryptCBC = runCBC cbcDecryptRound


challenge10 :: IO String
challenge10 = do
    ciphertext <- b64decodeFile "data/10.txt"
    return $ decryptCBC "YELLOW SUBMARINE" ciphertext
