module Set1.Challenge5 where

import Set1.Challenge2 (xor)
import Utils (asciiToHex, HexString)


repeatXorEncrypt :: String -> String -> HexString
repeatXorEncrypt key message = xor (asciiToHex paddedKey) (asciiToHex message)
    where paddedKey = take (length message) $ concat $ repeat key
