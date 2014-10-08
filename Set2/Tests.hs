module Set2.Tests where

import Test.HUnit

import Set2.Challenge9 (pkcs7Padding)


test9 :: Test
test9 = TestCase $ do
    let expected = "YELLOW SUBMARINE\EOT\EOT\EOT\EOT"
    expected @=? pkcs7Padding 20 "YELLOW SUBMARINE"


tests :: Test
tests = TestList [
    TestLabel "test9" test9]


main :: IO Counts
main = runTestTT tests
