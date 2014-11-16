-- The strategy is to use the average letter frequencies to score the decrypted
-- message for a key. If the letter frequency table of a decrypted message is
-- very similar to the letter frequency table of regular English text, then we
-- can assert with confidence that the key that was used is the one we are
-- looking for.
--
-- The way we quantify how "close" two frequency table are is by computing
-- their chi-square value. Thus, the key that produce a decrypted message whose
-- frequency table minimize the chi-square value (when computed against the
-- average English letter frequency) will be the desired key.

module Set1.Challenge3 where

import Data.Char (chr, ord, isLower, toLower)
import Data.Ord (comparing)
import Numeric (showHex)

import qualified Data.List as L
import qualified Data.Map.Strict as Map

import Set1.Challenge2 (xor)
import Utils (isPrint', unhexlify, bs2s, hexToAscii, HexString)


-- Average letter frequencies for English-language text.
-- The first number if for the letter 'a', the second for 'b', and so on until 'z'.
frequencies :: [Double]
frequencies = [
    8.2, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.1, 0.8, 4.0, 2.4, 6.7, 7.5,
    1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.1, 2.0, 0.1]


-- Compute the chi square value between the two lists.
chiSquare :: [Double] -> [Double] -> Double
chiSquare observed expected = sum $ zipWith f observed expected
    where f o e = ((o - e) ^ (2::Integer)) / e


-- Build a frequency table for the given text. The input string is assumed to
-- be composed of only characters in ['a'..'z']
makeFreqTable :: String -> [Double]
makeFreqTable s =
    let emptyCounter = Map.fromList $ zip ['a'..'z'] (repeat 0.0)
        counter = foldr (Map.adjust succ) emptyCounter s
    in
        map (/ fromIntegral (length s)) $ Map.elems counter


singleCharDecrypt :: Char -> HexString -> HexString
singleCharDecrypt c cipher =
    let
        key = concat $ replicate (length cipher) $ showHex (ord c) ""
    in
        xor key cipher


getChiSquareForKey :: Char -> HexString -> Double
getChiSquareForKey c cipher =
    let candidateSecret = unhexlify $ singleCharDecrypt c cipher
        cleanedSecret = filter isLower $ map toLower (bs2s candidateSecret)
        freqTable = makeFreqTable cleanedSecret
    in
        chiSquare freqTable frequencies


crackSingleByteXor :: HexString -> Char
crackSingleByteXor ciphertext =
    let candidateKeys = map chr [32..126]
        chis = zip candidateKeys $ map (`getChiSquareForKey` ciphertext) candidateKeys
        sortedChis = L.sortBy (comparing snd) chis

        -- Since we do not consider case when computing letter frequencies, it
        -- turns out that the chi-square value for a lower-case key is the same
        -- as the chi-square value for the corresponding upper-case key.
        -- To distinguish which one if the correct key, we decrypt the
        -- ciphertext with the first key we found and check if all the
        -- characters of the decrypted message are printable. If so, this is
        -- probably the right key, otherwise the second key in our sorted list
        -- of chi-square values is the right one.
        key1 = fst . head $ sortedChis
        key2 = fst . head . tail $ sortedChis
    in
        if all isPrint' . hexToAscii $ singleCharDecrypt key1 ciphertext
        then key1
        else key2
