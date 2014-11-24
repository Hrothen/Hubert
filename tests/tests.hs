{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (elem)
import Data.Either (either)
import Control.Monad (liftM)

import Test.HUnit

import Text.Parsec hiding (parseTest)
import Text.Parsec.Text

import Data.Maybe

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V


import Codec.Picture
import Codec.Picture.Types

import qualified HTML.Parser as PR
import qualified HTML.Parsec as PS

import Control.Lens

import Dom
import CSS
import Style
import Layout
import Painting


main = runTestTT tests

tests = TestList [TestLabel "ParserS html" htmlPR,
                  TestLabel "ParserS text" textPR,
                  TestLabel "ParserS elem" elemPR,
                  TestLabel "Parsec html"  htmlPS,
                  TestLabel "Parsec text"  textPS,
                  TestLabel "Parsec elem"  elemPS,
                  TestLabel "CSS sheet"    testCss,
                  TestLabel "Apply styles" testStyle,
                  TestLabel "Paint blocks" testPaint ]


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


------------------------------ CSS TESTS ----------------------------

testCss = parseTest "for valid css" sheet $ parseCSS css

------------------------------ STYLE TESTS --------------------------

testStyle = TestCase $ assertEqual "styletree" styletree $ styleTree dom css2 

----------------------------- PAINT TESTS ---------------------------

testPaint = TestCase $ do 
    testpng <- readPng "tests/rainbow.png"
    either (\_->assertFailure "missing png image")
           (compareImage paintpng)
           testpng

compareImage (Left e) _ = assertFailure $ T.unpack e
compareImage (Right i1) (ImageRGB8 i2) = do
  assertEqual "height" (imageHeight i1) (imageHeight i2)
  assertEqual "width"  (imageWidth i1)  (imageWidth i2)
  assertEqual "pixels" (imageData i1)   (imageData i2)

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
    span  = elem "span" (HM.singleton "id" "name") [text "world!"]
    p2    = elem "p"    (HM.singleton "class" "inner") [text "Goodbye!\n    "]



css = "h1, h2, h3 { margin: auto; color: #cc0000; }\n\
      \div.note { margin-bottom: 20px; padding: 10px; }\n\
      \#answer { display: none; }"

sheet = Stylesheet [ Rule [ Simple (Just "h1") Nothing []
                          , Simple (Just "h2") Nothing []
                          , Simple (Just "h3") Nothing [] ]
                          [ Declaration "margin" (Keyword "auto")
                          , Declaration "color"  (ColorValue(Color 204 0 0 255)) ]
                   , Rule [ Simple (Just "div") Nothing ["note"] ]
                          [ Declaration "margin-bottom" (Length 20 Px)
                          , Declaration "padding" (Length 10 Px) ]
                   , Rule [ Simple Nothing (Just "answer") [] ]
                          [ Declaration "display" (Keyword "none") ] ]


css2 = Stylesheet [ Rule [ Simple (Just "head") Nothing [] ]
                         [ Declaration "margin" (Keyword "auto")
                         , Declaration "color"  (ColorValue(Color 0 0 0 255)) ]
                  , Rule [ Simple (Just "p") Nothing ["inner"] ]
                         [ Declaration "padding" (Length 17 Px) ] ]

styletree = NTree (Element (ElementData "html" empt),empt) [head,p1,p2]
  where
    head    = NTree (Element (ElementData "head" empt),rule1) [title]
    title   = NTree (Element (ElementData "title" empt),empt) [test']
    test'   = NTree (Text "Test",empt) []
    p1      = NTree (Element (ElementData "p" (HM.singleton "class" "inner")),rule2) [hello,span]
    hello   = NTree (Text "Hello, ",empt) []
    span    = NTree (Element (ElementData "span" (HM.singleton "id" "name")),empt) [world]
    world   = NTree (Text "world!",empt) []
    p2      = NTree (Element (ElementData "p" (HM.singleton "class" "inner")),rule2) [goodbye]
    goodbye = NTree (Text "Goodbye!\n    ",empt) []
    empt    = HM.empty
    rule1   = HM.fromList [("margin",Keyword "auto"),("color",ColorValue (Color 0 0 0 255))]
    rule2   = HM.singleton "padding" (Length 17 Px)

contBlock = defaultDim & content.width.~800 & content.height.~168

paintpng = paintpng' s d
  where
    (Right d) = PS.parseHtml pnghtml
    (Right s) = parseCSS pngcss
    paintpng' s d = do
      let st = styleTree d s
      lyt <- layoutTree st contBlock
      let vec = pixels $ paint lyt (contBlock^.content)
      return $ generateImage (\x y-> c2px $ vec V.! (x + (y * 800))) 800 168
    c2px (Color r g b _) = PixelRGB8 r g b

pnghtml = "<div class=\"a\">\
\  <div class=\"b\">\
\    <div class=\"c\">\
\      <div class=\"d\">\
\        <div class=\"e\">\
\          <div class=\"f\">\
\            <div class=\"g\">\
\            </div>\
\          </div>\
\        </div>\
\      </div>\
\    </div>\
\  </div>\
\</div>"

pngcss = "* { display: block; padding: 12px; }\
\.a { background: #ff0000; }\
\.b { background: #ffa500; }\
\.c { background: #ffff00; }\
\.d { background: #008000; }\
\.e { background: #0000ff; }\
\.f { background: #4b0082; }\
\.g { background: #800080; }"
