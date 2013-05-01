
module Tools.Mill.Table where

import Data.ByteString
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString (GenParser)
import qualified Data.ByteString.Char8 as C
import Control.Applicative ((<$>), (<*>), (*>), (<*))

type Colname        = ByteString
type DataLine       = ByteString
type Header         = [Colname]

parseHeader :: GenParser Char st Header
parseHeader = (char '#' *> many1 colname)
    where colname = C.pack <$> (spaces *> many1 (noneOf " "))

