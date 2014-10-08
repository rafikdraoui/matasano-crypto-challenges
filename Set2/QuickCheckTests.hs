module Set2.QuickCheckTests where

import Test.QuickCheck

import Set1.QuickCheckTests (keyAndMessageGenerator)
import Set2.Challenge10 (encryptCBC, decryptCBC)


-- Properties

prop_CBC_encrypt_decrypt :: Property
prop_CBC_encrypt_decrypt = forAll keyAndMessageGenerator $
    \(key, s) -> (decryptCBC key . encryptCBC key) s == s

prop_CBC_decrypt_encrypt :: Property
prop_CBC_decrypt_encrypt = forAll keyAndMessageGenerator $
    \(key, s) -> (encryptCBC key . decryptCBC key) s == s


props :: [Property]
props = [
    prop_CBC_encrypt_decrypt,
    prop_CBC_decrypt_encrypt]


main :: IO ()
main = mapM_ quickCheck props
