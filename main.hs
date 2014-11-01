module Main where

import Data.HJSON
import System.Environment (getArgs)
import Data.Map ((!))
import Data.Attoparsec.Text.Lazy (parseOnly)
import Data.Text (pack)

-- this is not actually done lazily....
main :: IO ()
main = do
    f <- fmap head getArgs
    s <- case f of
        "-" -> getContents
        _ -> readFile f

    let v = parseOnly jValue . pack $ s
    let o = fmap toObject v
    let g = fmap (toObject . (! pack "glossary")) o
    let t = fmap (! pack "title") g

    print t
