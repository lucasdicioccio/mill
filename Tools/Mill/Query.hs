{-# LANGUAGE BangPatterns #-}

module Tools.Mill.Query 
    ( Query
    , parseQuery
    , passTest
    , Test (..)
    ) where

import Data.Either
import Tools.Mill.Table
import Text.Parsec.Prim
import Text.Parsec.Error
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.ByteString (GenParser)
import Data.Map
import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Data.ByteString(ByteString,hGetLine,hGetContents,pack,unpack)
import Data.ByteString.Lex.Double
import qualified Data.ByteString.Char8 as C

type QueryAtom      = (Colname, Test)

type Query          = Map Colname Test

data Test           = EqualString ByteString 
    | NotEqualString ByteString
    | GreaterEqual Double
    | GreaterThan Double
    | LowerEqual Double
    | LowerThan Double
    deriving (Show,Eq)

passTest ::  Test -> ByteString -> Bool
passTest (EqualString !s1)   s2 = {-# SCC "passTest.=" #-} s1 == s2
passTest (NotEqualString !s1)s2 = {-# SCC "passTest.!=" #-} s1 /= s2
passTest (GreaterEqual !s1)  s2 = {-# SCC "passTest.>=" #-} (read' s2) >= s1
passTest (GreaterThan !s1)   s2 = {-# SCC "passTest.>" #-} (read' s2) >  s1
passTest (LowerEqual !s1)    s2 = {-# SCC "passTest.<=" #-} (read' s2) <= s1
passTest (LowerThan !s1)     s2 = {-# SCC "passTest.<" #-} (read' s2) <  s1

read' :: ByteString -> Double
read' s = case unsafeReadDouble s of
            Nothing     -> error "cannot read as double"
            Just (d,_)  -> d

mergeAtoms :: [(Colname,Test)] -> Query
mergeAtoms = fromList

parseQuery ::  ByteString -> Either ParseError Query
parseQuery q = runParser queryParser () "" q

queryParser :: GenParser Char st Query
queryParser = mergeAtoms <$> many queryAtomParser

queryAtomParser :: GenParser Char st QueryAtom
queryAtomParser = (,) <$> colname <*> test

colname   = C.pack <$> many1 (noneOf "!=>< ")
test      = try equal
    <|> try notequal
    <|> try gte
    <|> try gt
    <|> try lte
    <|> try lt
    <|> fail "no such test"
equal     = EqualString . C.pack    <$> ((op "=")    *> opVal)
notequal  = NotEqualString . C.pack <$> ((op "!=")   *> opVal)
gte       = GreaterEqual . read     <$> ((op ">=")   *> number)
gt        = GreaterThan . read      <$> ((op ">")    *> number)
lte       = LowerEqual . read       <$> ((op "<=")   *> number)
lt        = LowerThan . read        <$> ((op "<")    *> number)
op v      = between (optional spaces) (optional spaces) (string v)
opVal     = many1 (noneOf " ")
number    = try stringFloat
    <|> try stringInt
    <?> "number"
stringInt = negativeInt <|> positiveInt
positiveInt = many1 digit
negativeInt = ('-':) <$> (char '-' *> positiveInt)
stringFloat = negativeFloat <|> positiveFloat
positiveFloat = do
    int <- many1 digit
    sep <- string "." <|> try (string "e-") <|> string "e" <?> "float form"
    frac <- many1 digit
    return $ concat [int, sep, frac]
negativeFloat = ('-':) <$> (char '-' *> positiveFloat)
