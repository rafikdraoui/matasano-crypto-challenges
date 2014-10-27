module Set2.QuickCheckTests where

import Control.Monad.Random (evalRandIO)
import Test.QuickCheck

import Set1.QuickCheckTests (keyAndMessageGenerator)
import Set2.Challenge10 (encryptCBC, decryptCBC)
import Set2.Challenge11 (cipherModeDetectorTest)


prop_CBC_encrypt_decrypt :: Property
prop_CBC_encrypt_decrypt = forAll keyAndMessageGenerator $
    \(key, s) -> (decryptCBC key . encryptCBC key) s == s


prop_CBC_decrypt_encrypt :: Property
prop_CBC_decrypt_encrypt = forAll keyAndMessageGenerator $
    \(key, s) -> (encryptCBC key . decryptCBC key) s == s


prop_cipher_mode_detector_is_always_right :: Property
prop_cipher_mode_detector_is_always_right = ioProperty $ do
    (guessedMode, usedMode) <- evalRandIO cipherModeDetectorTest
    return $ guessedMode == usedMode


props :: [Property]
props = [
    prop_CBC_encrypt_decrypt,
    prop_CBC_decrypt_encrypt,
    prop_cipher_mode_detector_is_always_right]


main :: IO ()
main = mapM_ quickCheck props
