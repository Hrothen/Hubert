{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (elem)

-- import qualified Distribution.TestSuite as TS
import Test.HUnit
import Text.Parsec
import Text.Parsec.Text

-- import Distribution.TestSuite (testGroup)

import qualified Data.Text as T
-- import qualified Data.Text.Encoding as E
import qualified Data.HashMap.Strict as HM

import qualified HTML.Parser as PR
import qualified HTML.Parsec as PS
import Dom


main = runTestTT tests

tests = TestList [TestLabel "ParserS html" htmlPR,
                  TestLabel "ParserS text" textPR,
                  TestLabel "ParserS elem" elemPR,
                  TestLabel "Parsec html"  htmlPS,
                  TestLabel "Parsec text"  textPS,
                  TestLabel "Parsec elem"  elemPS]


--------------------------- PARSER_S TESTS ------------------------------

parsePR p i = PR.runParserS p (PR.Parser i)

htmlPR = TestCase (assertEqual "for valid html" (Right dom) $
                                                 PR.parseHtml html)


textPR = TestCase (assertEqual "for valid text" testText $
                   parsePR PR.parseText "candygram")


elemPR = TestCase (assertEqual "for valid elem" testElem $
                   parsePR PR.parseElement "<p ham=\"doctor\">sup</p>")


---------------------------- PARSEC TESTS ------------------------------

instance Eq ParseError


htmlPS = TestCase (assertEqual "for valid html" (Right dom) $
                                                 PS.parseHtml html)


textPS = TestCase (assertEqual "for valid text" testText $
                   parse PS.parseText "" $ T.pack "candygram")


elemPS = TestCase (assertEqual "for valid elem" testElem $
                   parse PS.parseElement "" $ T.pack "<p ham=\"doctor\">sup</p>")


----------------------------- SHARED --------------------------------

testText = Right $ text "candygram"
testElem = Right $ elem "p" (HM.singleton "ham" "doctor") [text "sup"]

-- a small test html page
html = "<html>\n\
       \    <head>\n\
       \        <title>Test</title>\n\
       \    </head>\n\
       \    <p class=\"inner\">\n\
       \        Hello, <span id=\"name\">world!</span>\n\
       \    </p>\n\
       \    <p class=\"inner\">\n\
       \        Goodbye!\n\
       \    </p>\n\
       \</html>"

-- the expected result of parsing the test page
dom = elem "html" HM.empty [head,p1,p2]
  where
    head  = elem "head"  HM.empty [title]
    title = elem "title" HM.empty [text "Test"]
    p1    = elem "p"    (HM.singleton "class" "inner") [hello, span]
    hello = text "Hello, "
    span  = elem "span" (HM.singleton "id" "name") [text "world"]
    p2    = elem "p"    (HM.singleton "class" "inner") [text "Goodbye!"]