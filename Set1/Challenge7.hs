module Set1.Challenge7 where

import qualified Data.ByteString as B

import qualified Codec.Crypto.AES as AES

import Set1.Utils (bs2s, s2bs, b64decodeFile)


decrypt :: String -> String -> String
decrypt key cipher =
    let key' = s2bs key
        cipher' = s2bs cipher
        mode = AES.ECB
        direction = AES.Decrypt
        iv = B.replicate 16 0
    in
        bs2s $ AES.crypt' mode key' iv direction cipher'


challenge7 :: IO String
challenge7 = do
    cipher <- b64decodeFile "data/7.txt"
    return $ decrypt "YELLOW SUBMARINE" cipher
