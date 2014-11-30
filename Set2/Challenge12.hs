module Set2.Challenge12 where

import Control.Applicative ((<$>))
import Data.Char (chr)
import Data.List (group, inits)
import qualified Data.Map.Strict as Map

import Control.Monad.Random (evalRandIO)
import Control.Monad.Trans.State (evalState, get, put, State)

import Set1.Challenge8 (hasRepeatedBlocks)
import Set2.Challenge10 (encryptECB)
import Set2.Challenge11 (generateRandomBytes)
import Utils (b64decode)

-- "Global" values needed by several of the functions
data S = S {blockSize :: Int, knownBytes :: String}

type Oracle = String -> String

type OracleState = State S


encodedUnknownString :: String
encodedUnknownString = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg" ++
                       "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq" ++
                       "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg" ++
                       "YnkK"

makeOracle :: String -> String -> String
makeOracle key input =
    let unknownString = b64decode encodedUnknownString
        plaintext = input ++ unknownString
    in
        encryptECB key plaintext


findBlockSize :: Oracle -> Int
findBlockSize oracle =
    let inputs = inits $ repeat 'A'
        outputs = map oracle inputs
        (x:y:_) = group $ map length outputs
    in
        head y - head x


isUsingECB :: Oracle -> Bool
isUsingECB oracle =
    let input = replicate 500 'A'
        output = oracle input
    in
        hasRepeatedBlocks output


-- Return a padding string of a length such that when prepended to the known
-- bytes, it yields a string that is exactly one byte short of the block size.
makePadding :: OracleState String
makePadding = do
    s <- get
    let paddingLength = (-(length (knownBytes s) + 1)) `mod` blockSize s
    return $ replicate paddingLength 'A'

-- Build the crafted input for the character `c`
makeInput :: Char -> OracleState String
makeInput c = do
    s <- get
    padding <- makePadding
    return $ padding  ++ knownBytes s ++ [c]

-- Returns a mapping with keys being the encrypted oracle outputs of each
-- crafted padded input for characters 32 to 125, and values the input used for
-- the corresponding key.
makeMapping :: Oracle -> OracleState (Map.Map String String)
makeMapping oracle = do
    inputs <- mapM (makeInput . chr) [32..125]
    let items = zip (map oracle inputs) inputs
    return $ Map.fromList items

-- A Map lookup that only checks for the first `n` element of the given key.
-- Example:
--      lookupPrefix 2 "de" (Map.fromList [("abc", 1), ("def", 4)]) == Just 4
lookupPrefix :: Ord k => Int -> [k] -> Map.Map [k] a -> Maybe a
lookupPrefix n key mapping = Map.lookup (take n key) newMapping
    where newMapping = Map.mapKeys (take n) mapping


findNextByte :: Oracle -> OracleState (Maybe Char)
findNextByte oracle = do
    s <- get
    mapping <- makeMapping oracle
    padding <- makePadding
    let output = oracle padding
    let keyLength = 1 + length padding + length (knownBytes s)
    let input = lookupPrefix keyLength output mapping
    return $ last <$> input


gatherBytes :: Oracle -> OracleState String
gatherBytes oracle = do
    s <- get
    nextByte <- findNextByte oracle
    case nextByte of
        Nothing -> return $ knownBytes s
        Just c -> do
            put $ s {knownBytes = knownBytes s ++ [c]}
            gatherBytes oracle


crack' :: Oracle -> String
crack' oracle = evalState (gatherBytes oracle) initialState
    where initialState = S {blockSize = findBlockSize oracle, knownBytes = ""}

crack :: Oracle -> String
crack oracle =
    if isUsingECB oracle
    then crack' oracle
    else error "The black box is not using ECB mode"


main :: IO String
main = do
    key <- evalRandIO $ generateRandomBytes 16
    let oracle = makeOracle key
    return $ crack oracle
