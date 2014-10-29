{-# LANGUAGE OverloadedStrings #-}

module Data.HJSON where

import qualified Data.Map as M

import qualified Data.Attoparsec.Text.Lazy as ATL
import Data.Attoparsec.Text.Lazy (Parser)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TSR

import Data.Char (isSpace)

import Control.Applicative (Alternative(..))

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


jChar :: Parser Char
jChar = do
    ATL.choice [jCtrlSeq, ATL.satisfy (ATL.notInClass "\"\\")]


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
        otherwise -> fail "bad control sequence"


jHexChar :: Parser Char
jHexChar = toEnum `fmap` (read . ("0x" ++) . TS.unpack) `fmap` ATL.take 4


jString :: VParser
jString = do
    ATL.char '\"'
    s <- many jChar
    ATL.char '\"'
    return . VString . TS.pack $ s


jNumber :: VParser
jNumber = VNumber `fmap` ATL.double


-- here
{-
jObject :: VParser
jObject = do
    ATL.char '{'
    skipSpace
-}


jValue :: VParser
jValue = ATL.choice [jString, jNumber]

jParseTest :: String -> IO ()
jParseTest s = ATL.parseTest jValue (TL.pack s)
