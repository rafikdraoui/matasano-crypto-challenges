module Set1.Challenge8 where

import Control.Applicative ((<$>))
import Data.List (nub)

import Utils (splitInGroupsOf)


hasRepeatedBlocks :: String -> Bool
hasRepeatedBlocks s =
    let chunks = splitInGroupsOf 16 s
        uniqueChunks = nub chunks
    in
        length chunks /= length uniqueChunks


challenge8 :: IO String
challenge8 = do
    ciphertexts <- lines <$> readFile "data/8.txt"
    return $ head $ filter hasRepeatedBlocks ciphertexts
