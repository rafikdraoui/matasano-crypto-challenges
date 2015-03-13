module Set2.Tests where

import Test.HUnit

import Set2.Challenge9 (pkcs7Padding)
import Set2.Challenge15 (stripPadding)


test9 :: Test
test9 = TestCase $ do
    let expected = "YELLOW SUBMARINE\4\4\4\4"
    expected @=? pkcs7Padding 20 "YELLOW SUBMARINE"


test15 :: Test
test15 = TestCase $ do

    Right "ICE ICE BABY" @=? stripPadding "ICE ICE BABY\4\4\4\4"
    Right "" @=? stripPadding ""
    Right "YELLOW SUBMARINE" @=? stripPadding "YELLOW SUBMARINE"

    let invalidPadding = Left "Invalid padding"
    invalidPadding @=? stripPadding "ICE ICE BABY\5\5\5\5"
    invalidPadding @=? stripPadding "ICE ICE BABY\1\2\3\4"

    let invalidBlockSize = Left "Invalid block size"
    invalidBlockSize @=? stripPadding "ICE ICE BABY\3\3\3"
    invalidBlockSize @=? stripPadding "YELLOW SUBMARIN"
    invalidBlockSize @=? stripPadding "YELLOW SUBMARINES"


tests :: Test
tests = TestList [
    TestLabel "test9" test9,
    TestLabel "test15" test15]


main :: IO Counts
main = runTestTT tests
