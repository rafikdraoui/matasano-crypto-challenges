module Set2.Challenge9 where

import Data.Char (chr)


pkcs7Padding :: Int -> String -> String
pkcs7Padding blockSize s
    | length s `mod` blockSize == 0 = s
    | otherwise =
        let paddingLength = blockSize - (length s `mod` blockSize)
            padding = replicate paddingLength (chr paddingLength)
        in
            s ++ padding
