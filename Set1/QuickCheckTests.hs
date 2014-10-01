module Set1.QuickCheckTests where


import Control.Applicative ((<$>))
import Data.Char (isHexDigit, isUpper)
import qualified Data.ByteString as B

import Test.QuickCheck

import Set1.Utils (unhexlify, hexlify)


isLegalHexString :: String -> Bool
isLegalHexString s = even (length s) && all isHexDigit s && not (any isUpper s)

hexStringGen :: Gen String
hexStringGen = suchThat arbitrary isLegalHexString

byteStringGen :: Gen B.ByteString
byteStringGen = B.pack <$> vector 128


prop1 :: Property
prop1 = forAll hexStringGen $ \s ->  (hexlify . unhexlify) s == s

prop2 :: Property
prop2 = forAll byteStringGen $ \s -> (unhexlify . hexlify) s == s


main :: IO ()
main = mapM_ (quickCheckWith stdArgs { maxSuccess = 500 }) [prop1, prop2]
