module Set2.Challenge11 where

import Control.Monad (liftM, replicateM)
import Control.Monad.Random (Rand, RandomGen, getRandomR, uniform)

import Set1.Challenge7 (encryptECB)
import Set1.Challenge8 (hasRepeatedBlocks)
import Set1.Utils (splitInGroupsOf)
import Set2.Challenge9 (pkcs7Padding)
import Set2.Challenge10 (encryptCBC)


data EncryptionChoice = ECB | CBC
    deriving (Eq, Show)


generateRandomBytes :: RandomGen g => Int -> Rand g String
generateRandomBytes n = replicateM n $ getRandomR ('\0', '\255')


-- "Leaky" version of the oracle that also returns the EncryptionChoice that
-- has been made when encrypting the input. This is useful for testing our
-- cipher mode detector.
encryptionOracle' :: RandomGen g => String -> Rand g (String, EncryptionChoice)
encryptionOracle' input = do
    prefix <- getRandomR (5, 10) >>= generateRandomBytes
    suffix <- getRandomR (5, 10) >>= generateRandomBytes
    let plaintext = prefix ++ input ++ suffix
    let paddedPlaintext = concatMap (pkcs7Padding 16) $ splitInGroupsOf 16 plaintext

    key <- generateRandomBytes 16
    cipherMode <- uniform [ECB, CBC]
    let cipherFunction = if cipherMode == ECB then encryptECB else encryptCBC

    return (cipherFunction key paddedPlaintext, cipherMode)


-- Our real encryption oracle that only returns the ciphertext
encryptionOracle :: RandomGen g => String -> Rand g String
encryptionOracle input = liftM fst $ encryptionOracle' input


-- Taking an encryption oracle as an input, feed it a plaintext made of
-- identical blocks. If the output ciphertext contains repeated blocks, then
-- the block cipher mode probably was ECB. Otherwise, the mode used definitely
-- was CBC.
--
-- This relies on the fact that in ECB mode, identical plaintext blocks are
-- encrypted to identical ciphertext blocks.
cipherModeDetector :: RandomGen g => (String -> Rand g String) -> Rand g EncryptionChoice
cipherModeDetector oracle = do
    let input = replicate 500 'a'
    output <- oracle input
    return $ if hasRepeatedBlocks output then ECB else CBC


-- "Cheating" version of the detector using the leaky oracle for testing
-- purposes. It runs the experiment and returns a pair where the first element
-- is the guess cipher mode and the second element is the mode that was
-- actually used by the oracle.
cipherModeDetectorTest :: RandomGen g => Rand g (EncryptionChoice, EncryptionChoice)
cipherModeDetectorTest = do
    let input = replicate 500 'a'
    (output, mode) <- encryptionOracle' input
    return (if hasRepeatedBlocks output then ECB else CBC, mode)
