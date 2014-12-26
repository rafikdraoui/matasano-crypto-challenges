module Set2.Challenge14 where


import Control.Monad.Random (evalRandIO, getRandomR)
import Data.List (inits, group)

import Set2.Challenge11 (generateRandomBytes)
import qualified Set2.Challenge12 as C12
import Utils (splitInGroupsOf)


type Oracle = String -> String


makeOracle :: String -> String -> Oracle
makeOracle key padding input = C12.makeOracle key (padding ++ input)


{- Find sentinel block (i.e. the encrypted output of a '\0' block -}

getSentinelBlock :: Oracle -> String
getSentinelBlock oracle = findFirstRepeatedBlock $ oracle $ replicate 500 '\0'


findFirstRepeatedBlock :: String -> String
findFirstRepeatedBlock s =
    let chunks = group $ splitInGroupsOf 16 s
        repeatedChunks = filter (\g -> length g > 1) chunks
    in
        head . head $ repeatedChunks


-- Find the minimum input size needed to get a sentinel block in the output.
-- When this happens, the target string will be starting at the beginning of
-- a block.

-- E.g. If the padding string is "padding" and the target string is "SECRET",
-- then the required input size is 25:

--      padding000000000
--      0000000000000000
--      SECRET..........
getInputSizeUntilSentinel :: String -> Oracle -> Int
getInputSizeUntilSentinel sentinel oracle =
    let inputs = inits $ repeat '\0'
        outputs = map oracle inputs
    in
        length . fst . head $ filter (\p -> sentinel `blockIsIn` snd p) $ zip inputs outputs
    where
        needle `blockIsIn` haystack = needle `elem` splitInGroupsOf 16 haystack


-- Given an oracle, make a new oracle that
-- 1) prefixes its input with a '\0' string of the size required to get the
--    target string placed on a block boundary following a sentinel block;
-- 2) gets the output of the original oracle for the crafted input;
-- 3) drops all blocks from the output until (and including) the sentinel
--    block, and returns it.
--
-- This results in an oracle that is of the type of the one in Challenge 12
-- (i.e. no prefix padding, target string appended to input string)
--
-- Note: We need to pass in the input size and sentinel values as arguments,
-- since attempting to compute `getInputSizeUntilSentinel` within this function
-- leads to the program hanging...
makeNewOracle :: Int -> String -> Oracle -> Oracle
makeNewOracle n sentinel oracle input =
    let
        output = oracle $ replicate n '\0' ++ input
    in
        concat . tail . dropWhile (/= sentinel) $ splitInGroupsOf 16 output


crack :: Oracle -> String
crack oracle =
    let sentinel = getSentinelBlock oracle
        n = getInputSizeUntilSentinel sentinel oracle
        newOracle = makeNewOracle n sentinel oracle
    in
        C12.crack newOracle


main :: IO String
main = do
    key <- evalRandIO $ generateRandomBytes 16
    padding <- evalRandIO $ getRandomR (5, 150) >>= generateRandomBytes
    let oracle = makeOracle key padding
    return $ crack oracle
