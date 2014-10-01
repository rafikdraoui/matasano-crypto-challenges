module Set1.Challenge6 where

import Control.Applicative ((<$>))
import Data.Bits (popCount, xor)
import Data.Char (ord)
import Data.List (minimumBy, transpose)
import Data.Ord (comparing)

import Set1.Challenge3 (crackSingleByteXor)
import Set1.Challenge5 (repeatXorEncrypt)
import Set1.Utils (asciiToHex, hexToAscii, splitInGroupsOf, b64decode)


numDifferingBits :: Int -> Int -> Int
numDifferingBits x y = popCount $ x `xor` y


hammingDistance :: String -> String -> Int
hammingDistance x y =
    let x' = map ord x
        y' = map ord y
    in
        sum $ zipWith numDifferingBits x' y'


normalizeBy :: Int -> Int -> Double
normalizeBy keySize score = fromIntegral score / fromIntegral keySize


-- The score for a given keySize is computed by splitting the cipher text in
-- chunks of length `keySize`, then computing the hamming distance (normalized
-- by `keySize`) between each consecutive chunks, and taking the average.
getScoringForKeySize :: Int -> String -> Double
getScoringForKeySize keySize cipher =
    let chunks = splitInGroupsOf keySize cipher
        chunks' = if length (last chunks) /= keySize then init chunks else chunks
        pairs = splitInGroupsOf 2 chunks'
        pairs' = if length (last pairs) /= 2 then init pairs else pairs
        scores = map (\p -> hammingDistance (head p) (last p)) pairs'
        scores' = map (normalizeBy keySize) scores
    in
        sum scores' / fromIntegral (length scores')


-- The key size is the one with the minimum score
findKeySize :: String -> Int
findKeySize cipher =
    let candidates = [2..42]
        scores = zip candidates $ map (`getScoringForKeySize` cipher) candidates
    in
        fst $ minimumBy (comparing snd) scores


challenge6 :: IO (String, String)
challenge6 = do
    cipher <- b64decode . concat . lines <$> readFile "Set1/data/1.6.txt"
    let keySize = findKeySize cipher
    let transposedBlocks = transpose $ splitInGroupsOf keySize cipher
    let key = map (crackSingleByteXor . asciiToHex) transposedBlocks
    let secret = hexToAscii $ repeatXorEncrypt key cipher
    return (key, secret)
