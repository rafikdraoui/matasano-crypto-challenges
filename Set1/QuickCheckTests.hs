module Set1.QuickCheckTests where


import Control.Applicative ((<$>))
import Data.Char (isHexDigit, isUpper)
import qualified Data.ByteString as B

import Test.QuickCheck

import Set1.Challenge7 (encryptECB, decryptECB)
import Utils (unhexlify, hexlify)


-- Generators

isLegalHexString :: String -> Bool
isLegalHexString s = even (length s) && all isHexDigit s && not (any isUpper s)

hexStringGen :: Gen String
hexStringGen = suchThat arbitrary isLegalHexString

byteStringGen :: Gen B.ByteString
byteStringGen = B.pack <$> vector 128


blockGen :: Gen String
blockGen = suchThat arbitrary $ (==16) . length

sizedStringGen :: Gen String
sizedStringGen = do
    Positive numBlocks <- arbitrary
    blocks <- vectorOf numBlocks blockGen
    return $ concat blocks

keyAndMessageGenerator :: Gen (String, String)
keyAndMessageGenerator = do
    key <- blockGen
    message <- sizedStringGen
    return (key, message)


-- Properties

prop_unhexlify_hexlify :: Property
prop_unhexlify_hexlify = forAll hexStringGen $
    \s ->  (hexlify . unhexlify) s == s

prop_hexlify_unhexlify :: Property
prop_hexlify_unhexlify = forAll byteStringGen $
    \s -> (unhexlify . hexlify) s == s


prop_ECB_encrypt_decrypt :: Property
prop_ECB_encrypt_decrypt = forAll keyAndMessageGenerator $
    \(key, s) -> (decryptECB key . encryptECB key) s == s

prop_ECB_decrypt_encrypt :: Property
prop_ECB_decrypt_encrypt = forAll keyAndMessageGenerator $
    \(key, s) -> (encryptECB key . decryptECB key) s == s


props :: [Property]
props = [
    prop_unhexlify_hexlify,
    prop_hexlify_unhexlify,
    prop_ECB_encrypt_decrypt,
    prop_ECB_decrypt_encrypt]


main :: IO ()
main = mapM_ quickCheck props
