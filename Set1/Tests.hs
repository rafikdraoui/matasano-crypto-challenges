module Set1.Tests where

import Test.HUnit

import Set1.Challenge1 (hexToBase64)
import Set1.Challenge2 (xor)
import Set1.Challenge3 (crackSingleByteXor, singleCharDecrypt)
import Set1.Challenge4 (challenge4)
import Set1.Challenge5 (repeatXorEncrypt)
import Set1.Challenge6 (challenge6, hammingDistance)
import Utils (hexToAscii, isPrint')


test1 :: Test
test1 = TestCase $ do
    let s = "49276d206b696c6c696e6720796f757220627261696e206c" ++
            "696b65206120706f69736f6e6f7573206d757368726f6f6d"
    let expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    expected @=? hexToBase64 s


test2 :: Test
test2 = TestCase $ do
    let x = "1c0111001f010100061a024b53535009181c"
    let y = "686974207468652062756c6c277320657965"
    let expected = "746865206b696420646f6e277420706c6179"
    expected @=? xor x y


test3 :: Test
test3 = TestCase $ do
    let cipher = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

    let foundKey = crackSingleByteXor cipher
    let expectedKey = 'X'
    expectedKey @=? foundKey

    let foundSecret = hexToAscii $ singleCharDecrypt foundKey cipher
    let expectedSecret = "Cooking MC's like a pound of bacon"
    expectedSecret @=? foundSecret

test4 :: Test
test4 = TestCase $ do
    result <- challenge4
    let expected = ("7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f", "Now that the party is jumping\n")
    expected @=? result


test5 :: Test
test5 = TestCase $ do
    let secret = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    let expected = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

    expected @=? repeatXorEncrypt "ICE" secret


test6 :: Test
test6 = TestCase $ do
    -- Hamming Distance
    37 @=? hammingDistance "this is a test" "wokka wokka!!!"

    (key, secret) <- challenge6
    let expectedKey = "Terminator X: Bring the noise"
    expectedKey @=? key
    assertBool "Unprintable characters in secret" $ all isPrint' secret


tests :: Test
tests = TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6]


main :: IO Counts
main = runTestTT tests
