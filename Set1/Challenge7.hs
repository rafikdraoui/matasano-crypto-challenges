module Set1.Challenge7 where

import qualified Data.ByteString as B

import qualified Codec.Crypto.AES as AES

import Utils (bs2s, s2bs, b64decodeFile)


runECB :: AES.Direction -> String -> String -> String
runECB dir key cipher =
    let key' = s2bs key
        cipher' = s2bs cipher
        mode = AES.ECB
        iv = B.replicate 16 0
    in
        bs2s $ AES.crypt' mode key' iv dir cipher'


encryptECB :: String -> String -> String
encryptECB = runECB AES.Encrypt

decryptECB :: String -> String -> String
decryptECB = runECB AES.Decrypt


challenge7 :: IO String
challenge7 = do
    cipher <- b64decodeFile "data/7.txt"
    return $ decryptECB "YELLOW SUBMARINE" cipher
