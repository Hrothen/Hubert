{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (elem)
import Data.Either (either)

-- import qualified Distribution.TestSuite as TS
import Test.HUnit
import Text.Parsec hiding (parseTest)
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

htmlPR = parseTest "for valid html" dom $ PR.parseHtml html


textPR = parseTest "for valid text" testText $ parsePR PR.parseText "candygram"


elemPR = parseTest "for valid elem" testElem $
                   parsePR PR.parseElement "<p ham=\"doctor\">sup</p>"


---------------------------- PARSEC TESTS ------------------------------


htmlPS = parseTest "for valid html" dom $ PS.parseHtml html


textPS = parseTest "for valid text" testText $
                    parse PS.parseText "" $ T.pack "candygram"


elemPS = parseTest "for valid elem" testElem $
                    parse PS.parseElement "" $ T.pack "<p ham=\"doctor\">sup</p>"


----------------------------- SHARED --------------------------------

-- generic test: given an expected value and an actual value, check that the actual
-- value is not an error message, then compare it to the expected value
parseTest msg e = TestCase . either (assertFailure . show) (assertEqual msg e)

testText = text "candygram"
testElem = elem "p" (HM.singleton "ham" "doctor") [text "sup"]

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