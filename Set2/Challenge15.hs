module Set2.Challenge15 where

-- Assumption: The padding is for a block size of 16

import Data.Char (chr, ord)
import Data.List (tails)

import Utils (splitInGroupsOf)


hasValidPaddingChar :: String -> Bool
hasValidPaddingChar s = not (null s) && last s `elem` map chr [1..15]


-- Assumption: not (null s)
allSamePaddingChar :: String -> Bool
allSamePaddingChar s =
    let lastBlock = last $ splitInGroupsOf 16 s
        paddingChar = last lastBlock
        padding = head $ filter (all (== paddingChar)) $ tails lastBlock
    in
        length padding == ord paddingChar


-- Assumption: not (null s)
stripPadding' :: String -> String
stripPadding' s = take (length s - paddingLength) s
    where paddingLength = ord (last s)


stripPadding :: String -> Either String String
stripPadding s
    -- If the input length is not a multiple of the block size, we return
    -- an error.
    | length s `mod` 16 /= 0 = Left "Invalid block size"

    -- If the input does not end with a valid padding character, then it has no
    -- padding (and does not need any, since its length is a multiple of the
    -- block size), so we just return it.
    | not (hasValidPaddingChar s) = Right s

    -- If the input does end with a valid padding character, then we check that
    -- the padding is of the right size and has the right byte value, and then
    -- return its stripped value.
    | allSamePaddingChar s = Right $ stripPadding' s

    -- Otherwise, the padding was invalid, and we return an error.
    | otherwise = Left "Invalid padding"
