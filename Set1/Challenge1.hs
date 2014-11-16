module Set1.Challenge1 where

import qualified Data.ByteString.Base64 as B64

import Utils (bs2s, unhexlify, HexString)


hexToBase64 :: HexString -> String
hexToBase64 = bs2s . B64.encode . unhexlify
