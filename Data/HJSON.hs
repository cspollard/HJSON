{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.HJSON where

import qualified Data.Map as M

import qualified Data.Attoparsec.Text.Lazy as ATL
import Data.Attoparsec.Text.Lazy (Parser, skipSpace)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Control.Applicative (Alternative(..), Applicative(..))

type Object = M.Map TS.Text Value

data Value =
    VString TS.Text
    | VNumber Double
    | VObject Object
    | VArray [Value]
    | VBool Bool
    | VNull
    deriving Show

type VParser = Parser Value


toText :: Value -> TS.Text
toText (VString t) = t
toText _ = undefined


toObject :: Value -> Object
toObject (VObject o) = o
toObject _ = undefined


jChar :: Parser Char
jChar = ATL.choice [jCtrlSeq, ATL.satisfy (ATL.notInClass "\"\\")]


jCtrlSeq :: Parser Char
jCtrlSeq = do
    ATL.char '\\'
    c <- ATL.satisfy (ATL.inClass "\"\\/bfnrtu")
    case c of
        '\"' -> return '\"'
        '\\' -> return '\\'
        '/' -> return '/'
        'b' -> return '\b'
        'f' -> return '\f'
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        'u' -> jHexChar
        _ -> fail "bad control sequence"


-- TOOD
-- probably slow
jHexChar :: Parser Char
jHexChar = toEnum `fmap` (read . ("0x" ++) . TS.unpack) `fmap` ATL.take 4


jString :: VParser
jString = do
    ATL.char '\"'
    s <- many jChar
    ATL.char '\"'
    skipSpace

    return . VString . TS.pack $ s


jNumber :: VParser
jNumber = VNumber `fmap` ATL.double


jNameValPair :: Parser (TS.Text, Value)
jNameValPair = do
    n <- jString
    skipSpace
    ATL.char ':'
    skipSpace
    v <- jValue
    return (toText n, v)


jObject :: VParser
jObject = do
    ATL.char '{'
    skipSpace
    pairs <- ATL.sepBy jNameValPair (ATL.char ',' *> skipSpace)
    skipSpace
    ATL.char '}'
    return . VObject $ M.fromList pairs


jArray :: VParser
jArray = do
    ATL.char '['
    skipSpace
    vs <- ATL.sepBy jValue (ATL.char ',' *> skipSpace)
    skipSpace
    ATL.char ']'
    return $ VArray vs


jTrue :: VParser
jTrue = ATL.string "true" *> return (VBool True)

jFalse :: VParser
jFalse = ATL.string "false" *> return (VBool False)


jNull :: VParser
jNull = ATL.string "null" *> return VNull


jValue :: VParser
jValue = ATL.choice [jString, jNumber, jObject, jArray, jTrue, jFalse, jNull]

jParseTest :: String -> IO ()
jParseTest s = ATL.parseTest jValue (TL.pack s)

jParseTestFile :: String -> IO ()
jParseTestFile f = do
    s <- TLIO.readFile f
    ATL.parseTest jValue s
