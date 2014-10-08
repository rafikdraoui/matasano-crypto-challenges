module Set1.Challenge4 where

import Control.Applicative ((<$>))

import Set1.Challenge3 (crackSingleByteXor, singleCharDecrypt)
import Set1.Utils (isPrint', hexToAscii, HexString)


findSecret :: [HexString] -> (HexString, String)
findSecret ciphertexts =
    let keys = map crackSingleByteXor ciphertexts
        candidateSecrets = zipWith singleCharDecrypt keys ciphertexts
        candidateSecrets' = zip ciphertexts (map hexToAscii candidateSecrets)
    in
        head $ filter (all isPrint' . snd) candidateSecrets'


challenge4 :: IO (HexString, String)
challenge4 = do
    ciphertexts <- lines <$> readFile "data/4.txt"
    return $ findSecret ciphertexts
