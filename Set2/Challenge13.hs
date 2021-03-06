module Set2.Challenge13 where

import Control.Applicative ((<$>))
import Data.Char (ord)
import qualified Data.Map.Strict as Map

import Set2.Challenge9 (pkcs7Padding)
import Set2.Challenge10 (encryptECB, decryptECB)
import Utils (replace, splitOn)


data Profile = Profile {
    email :: String,
    uid :: String,
    role :: String
} deriving (Show)


{- Profile encoding -}

-- URL-quote escaping for characters '=' and '&'
escape :: String -> String
escape = replace '=' "%3D" . replace '&' "%26"

encode :: Profile -> String
encode p = "email=" ++ escape (email p) ++
           "&uid=" ++ escape (uid p) ++
           "&role=" ++ escape (role p)


{- Profile decoding -}

isValid :: String -> Bool
isValid s =
    let s' = splitOn '=' <$> splitOn '&' s
    in all (\x -> length x == 2) s'

-- Returns `val` wrapped in Just if `cond` is True, otherwise returns Nothing
maybeValue :: Bool -> a -> Maybe a
maybeValue cond val = if cond then Just val else Nothing

-- Decode the string to a "raw" key-value mapping
decode' :: String -> Maybe (Map.Map String String)
decode' s = maybeValue (isValid s) $
    let keyValues = splitOn '=' <$> splitOn '&' s

        -- The pattern-match will never fail since `isValid s == True`
        keyValueTuples = map (\[x, y] -> (x, y)) keyValues
    in
        Map.fromList keyValueTuples

decode :: String -> Maybe Profile
decode s = do
    p <- decode' s
    e <- Map.lookup "email" p
    u <- Map.lookup "uid" p
    r <- Map.lookup "role" p
    return $ Profile e u r


{- Challenge 13 -}

-- We always use uid=10, but the attack could be easily adapted if the values
-- for uid where an increasing sequence of integers.
profileFor :: String -> String -> String
profileFor key e = encryptECB key . encode $ Profile e "10" "user"

-- This is not robust, but this does the trick in our case
removePadding :: String -> String
removePadding s = take (length s - paddingLength) s
    where paddingLength = ord (last s)

getEncryptedProfile :: String -> String -> Maybe Profile
getEncryptedProfile key = decode . removePadding . decryptECB key


{-
TL;DR: We use the encrypted outputs of

email=a@example.            email=aaaaaaaaaa
com&uid=10&role=     and    admin...........
user                        &uid=10&role=use
                            r

to build the encrypted output of the admin profile

email=a@example.
com&uid=10&role=
admin...........

More info in the comment at the bottom of the file.
-}
main :: Maybe Profile
main =
    let key = "YELLOW SUBMARINE"
        prefix = take 32 $ profileFor key "a@example.com"
        suffix = take 16 . drop 16 . profileFor key $
            "aaaaaaaaaa" ++ pkcs7Padding 16 "admin"
    in
        getEncryptedProfile key $ prefix ++ suffix



{-
The idea is to craft an input email `e` such that the encoded profile
    email=<e>&uid=10&role=user
has the last `=` (between "role" and "user") ending at a position that is a
multiple of the block size. In our case, we want that character to be at
position 16, or 32, or 48, etc. When this is the case, the value for the role
(usually "user") will be encrypted as a single block on its own.

Since ECB blocks are encrypted independently from each other, we can take the
first encrypted blocks corresponding to `email=<e>&uid=10&role=` (1) and append
to it the single encrypted block for the word "admin" (2) to get a valid
ciphertext that will be decrypted as the encoded profile
`email=<e>&uid=10&role=admin`

(1)
The combined length of all the characters in the encoded profile other than the
email and role values is
     sum $ map length ["email=", "&uid=10", "&role="] = 19
So we would like an email value that makes the offset from the role value to be
32 (i.e. the next highest multiple of 16, the block size), which means we need
an email of length 32 - 19 = 13. The email "a@example.com" has the
required length.

(2)
We want to craft an email input so that the PKCS#7-padded string "admin" appears
on a single block. We want something like

email=aaaaaaaaaa
admin...........
&uid=10&role=use
r

where each line is a block of length 16, and each '.' is the appropriate
padding character.
-}
