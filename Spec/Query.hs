{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Tools.Mill.Query
import qualified Data.Map as M
import Text.Parsec.Error

instance Eq ParseError

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

str `shouldParseAs` val = parseQuery str `shouldBe` Right val

str `shouldParseAsQuery` val = str `shouldParseAs` M.fromList val

main :: IO ()
main = hspec $ do
    describe "Query.parseQuery" $ do
        it "returns an empty query on an empty string" $ do
            "" `shouldParseAs` M.empty

        it "can parse basic queries" $ do
            "foo=bar" `shouldParseAsQuery` [("foo",EqualString "bar")]
            "foo!=bar" `shouldParseAsQuery` [("foo",NotEqualString "bar")]
            "foo>20" `shouldParseAsQuery` [("foo",GreaterThan 20)]
            "foo>=20" `shouldParseAsQuery` [("foo",GreaterEqual 20)]
            "foo<20" `shouldParseAsQuery` [("foo",LowerThan 20)]
            "foo<=20" `shouldParseAsQuery` [("foo",LowerEqual 20)]

        it "can parse basic queries with a space" $ do
            "foo = bar" `shouldParseAsQuery` [("foo",EqualString "bar")]
            "foo != bar" `shouldParseAsQuery` [("foo",NotEqualString "bar")]
            "foo > 20" `shouldParseAsQuery` [("foo",GreaterThan 20)]
            "foo < 20" `shouldParseAsQuery` [("foo",LowerThan 20)]
            "foo >= 20" `shouldParseAsQuery` [("foo",GreaterEqual 20)]
            "foo <= 20" `shouldParseAsQuery` [("foo",LowerEqual 20)]
            "foo= bar" `shouldParseAsQuery` [("foo",EqualString "bar")]
            "foo =bar" `shouldParseAsQuery` [("foo",EqualString "bar")]
            "foo    =   bar" `shouldParseAsQuery` [("foo",EqualString "bar")]

        it "can read positive ints" $ do
            "foo > 20" `shouldParseAsQuery` [("foo",GreaterThan 20)]

        it "can read negative ints" $ do
            "foo > -20" `shouldParseAsQuery` [("foo",GreaterThan (-20))]

        it "can read positive floating points" $ do
            "foo > 5.3" `shouldParseAsQuery` [("foo",GreaterThan 5.3)]

        it "can read negative floating points" $ do
            "foo > -5.3" `shouldParseAsQuery` [("foo",GreaterThan (-5.3))]

        it "can read positive exponent floating points" $ do
            "foo > 1e3" `shouldParseAsQuery` [("foo",GreaterThan 1000)]

        it "can read negative exponent floating points" $ do
            "foo > 1e-1" `shouldParseAsQuery` [("foo",GreaterThan 0.1)]

        it "returns an error when asking for larger/smaller than non-numbers" $ do
            parseQuery "foo > bar" `shouldSatisfy` isLeft
            parseQuery "foo < bar" `shouldSatisfy` isLeft
            parseQuery "foo >= bar" `shouldSatisfy` isLeft
            parseQuery "foo >= bar" `shouldSatisfy` isLeft
