module Set1.Challenge2 where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B

import Utils (asciiToHex, hexToAscii, hexlify, unhexlify, HexString)


xor :: HexString -> HexString -> HexString
xor x y = hexlify $ B.pack $ B.zipWith Bits.xor x' y'
    where x' = unhexlify x
          y' = unhexlify y


xor' :: String -> String -> String
xor' x y = hexToAscii $ xor (asciiToHex x) (asciiToHex y)
