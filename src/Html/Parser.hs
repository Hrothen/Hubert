module HTML where

import Data.List (isPrefixOf)
import Data.Word (Word(..))
import Data.Char (isAlphaNum)
import Control.Monad (liftM)

import Control.Monad.State.Lazy (StateT(..), State, modify, evalState, get, put)
import Control.Monad.Except (ExceptT(..), Except, runExceptT, throwError)
import Control.Monad.Identity

import qualified Data.HashMap.Strict as HM

import Dom

data Parser = Parser Word String

type ParserS = ExceptT String (StateT Parser Identity)

-- runParserS :: ParserS a -> Parser -> a
runParserS p s = evalState (runExceptT p) s

nextchr :: Parser -> Char
nextchr (Parser pos input) = input!!(fromIntegral pos)

startsWith :: Parser -> String -> Bool
startsWith (Parser pos input) s = s `isPrefixOf` drop (fromIntegral pos) input

eof :: Parser -> Bool
eof (Parser pos input) = (fromIntegral pos) >= length input

increment :: Word -> ParserS ()
increment i = modify (\(Parser p inp)-> Parser (p+i) inp)

consumeChar :: ParserS Char
-- consumeChar p@(Parser pos input) = (Parser (pos+1) input,nextchr p)
consumeChar = do
    psr <- get
    let p = nextchr psr
    increment 1
    return p

consumeWhile :: (Char -> Bool) -> ParserS String
consumeWhile f = do
    Parser pos input <- get
    let s = takeWhile f $ drop (fromIntegral pos) input
        len = fromIntegral $ length s
    increment len
    return s

consumeWhitespace :: ParserS String
consumeWhitespace = consumeWhile (==' ')

parseTagName :: ParserS String
parseTagName = consumeWhile isAlphaNum


-- use this to mimic rominson's (improper, soon to be depriciated)
-- use of assert
assert :: String -> Bool -> ParserS ()
assert s b = if b then return () else throwError s


parseNode :: ParserS Node
parseNode = do
    p <- get
    if nextchr p == '<' then parseElement else parseText

parseText :: ParserS Node
parseText = liftM Dom.text $ consumeWhile (/='<')

parseElement :: ParserS Node
parseElement = do
    consumeChar >>= assert "missing < in open tag" . (=='<')
    tag <- parseTagName
    attrs <- parseAttributes
    consumeChar >>= assert "missing > in open tag" . (=='>')
    -- contents
    children <- parseNodes
    --end tag
    consumeChar  >>= assert "missing < in close tag" . (=='<')
    consumeChar  >>= assert "missing / in close tag" . (=='/')
    parseTagName >>= assert "end tag doesn't match start tag" . (==tag)
    consumeChar  >>= assert "missing > in close tag" . (=='>')

    return $ Dom.elem tag attrs children


parseAttr :: ParserS (String, String)
parseAttr = do
    name <- parseTagName
    consumeChar >>= assert "missing =" . (=='=')
    value <- parseAttrValue
    return (name,value)

parseAttrValue :: ParserS String
parseAttrValue = do
    open <- consumeChar
    assert "invalid open" (open == '"' || open == '\'')
    val <- consumeWhile (/=open)
    consumeChar >>= assert "invalid close" . (==open)
    return val

parseAttributes :: ParserS AttrMap
parseAttributes = parseAttributes' HM.empty
  where
    parseAttributes' attrs = do
        consumeWhitespace
        p <- get
        if nextchr p == '>' then return attrs
        else do
            (name,val) <- parseAttr
            parseAttributes' $ HM.insert name val attrs


parseNodes :: ParserS [Node]
parseNodes = parseNodes' []
  where
    parseNodes' nodes = do
        consumeWhitespace
        p <- get
        if eof p || p `startsWith` "</"
        then return nodes
        else parseNode >>= parseNodes' . (nodes++) . (:[])  --slow for big DOM


parseHtml :: String -> Either String Node
parseHtml s = case runParserS parseNodes (Parser 0 s) of
              Left err -> Left err
              Right nodes -> Right $
                if length nodes == 1
                then head nodes
                else Dom.elem "html" HM.empty nodes
