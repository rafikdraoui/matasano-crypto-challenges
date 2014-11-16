module Utils where

import Control.Applicative ((<$>))
import Data.Char (chr, isPrint, isSpace, ord)
import Data.Word (Word8)
import Numeric (readHex, showHex)
import qualified Data.ByteString as B

import qualified Data.ByteString.Base64 as B64


-- Type alias to represent an hexadecimal string.
-- This won't be enforced by the compiler, it's meant more as an aid for me to
-- keep things straight.
type HexString = String


-- Various conversion functions between (ASCII) String, HexString and ByteString

bs2s :: B.ByteString -> String
bs2s s = [chr (fromIntegral b::Int) | b <- B.unpack s]

s2bs :: String -> B.ByteString
s2bs = foldr (B.cons . fromIntegral . ord) B.empty


word8ToHex :: Word8 -> HexString
word8ToHex c
    | c < 16 = "0" ++ showHex c ""
    | otherwise = showHex c ""

hexlify :: B.ByteString -> HexString
hexlify s = concatMap word8ToHex $ B.unpack s

unhexlify :: HexString -> B.ByteString
unhexlify [] = B.empty
unhexlify [_] = error "Odd number of bytes"
unhexlify (x:y:rest) = B.cons b (unhexlify rest)
    where [(b, _)] = readHex [x, y]


asciiToHex :: String -> HexString
asciiToHex = hexlify . s2bs

hexToAscii :: HexString -> String
hexToAscii = bs2s . unhexlify


-- For some reason, Data.Char.isPrint '\n' == False, despite the documentation
-- mentioning that isPrint "selects printable Unicode characters (letters,
-- numbers, marks, punctuation, symbols and spaces)". So we provide a
-- customized version.
isPrint' :: Char -> Bool
isPrint' c = isPrint c || isSpace c


-- Split a list of objects in groups whose size are given by the first
-- argument. The last group can be smaller if the given size does not divide
-- the length of the input list.
--
-- Example: splitInGroupsOf 3 [1..8] == [[1,2,3],[4,5,6],[7,8]]
splitInGroupsOf :: Int -> [a] -> [[a]]
splitInGroupsOf n x
    | n < 1 || null x = []
    | otherwise = prefix : splitInGroupsOf n rest
        where (prefix, rest) = splitAt n x


b64decode :: String -> String
b64decode s = case B64.decode (s2bs s) of
    Left e -> error $ "b64decode: " ++ e
    Right x -> bs2s x


b64decodeFile :: FilePath -> IO String
b64decodeFile filename = b64decode . concat . lines <$> readFile filename
