{-# LANGUAGE BangPatterns #-}
module Tools.Mill (main) where

import Data.Map (unions)
import Data.Either (rights)
import Control.Monad (forM_,forever,replicateM_)
import Tools.Mill.Table
import Tools.Mill.Query
import Text.Parsec.Prim
import qualified Data.ByteString.Char8 as C
import Data.Map(fromList,lookup)
import Prelude hiding (lookup)
import System.Environment (getArgs)
import System.IO(stdin,stderr)
import System.IO(hPutStrLn)
import Data.ByteString(ByteString,hGetLine,hGetContents,pack,unpack)
import Control.Concurrent
import Control.Concurrent.MVar

match :: Header -> Query -> DataLine -> Bool
match header qs = match' $ map ((flip lookup) qs) header 

match' tests line = all passTest' $ zip tests cols
    where cols                      = C.split ' ' line
          passTest' (Nothing,  _)   = True
          passTest' ((Just t), v)   = passTest t v

infoMode keys = do
    headerString <- hGetLine stdin
    let parsedHeader = runParser parseHeader () "" headerString
    either wrongHeader (\_ -> print headerString) parsedHeader
    where  wrongHeader _ = hPutStrLn stderr "wrong Header"

filterMode keys = do
    let query = unions $ rights $ map parseQuery keys
    hPutStrLn stderr $ show query
    headerString <- hGetLine stdin
    let parsedHeader = runParser parseHeader () "" headerString
    either wrongHeader (filterBody headerString query) parsedHeader
    where  wrongHeader _ = hPutStrLn stderr "wrong Header"
           filterBody headerString query header = do
                C.putStrLn headerString
                body <- hGetContents stdin
                let matcher = match header query
                forM_ (filter matcher (C.split '\n' body)) $ C.putStrLn

main = do
    args <- getArgs
    case args of
        ("--infos"   : keys)    -> infoMode keys
        ("--filter"  : keys)    -> filterMode $ map C.pack keys
        otherwise               -> print usage

usage = "--filter [queries] | --infos"

