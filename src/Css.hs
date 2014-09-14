{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module CSS
    ( Stylesheet(..)
    , Rule(..)
    , Selector(..)
    , Declaration(..)
    , Value(..)
    , Unit(..)
    , parseCSS
    , selectors
    , declarations
    ) where

import Prelude hiding (id)

import Data.Word (Word(..), Word8(..))
import Data.List (sortBy)
import Data.Maybe (maybe)
import Numeric (readFloat, readHex)
import Control.Applicative ((<*), (*>), (<$>), liftA, liftA2)

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

data Stylesheet = Stylesheet [Rule]
  deriving (Show, Eq)

data Rule = Rule [Selector] [Declaration]
  deriving (Show, Eq)

-- only handle simple selectors for now
data Selector = Simple (Maybe T.Text) (Maybe T.Text) [T.Text]
  deriving (Show, Eq)

data Declaration = Declaration T.Text Value
  deriving (Show, Eq)

data Value = Keyword T.Text
           | Color Word8 Word8 Word8 Word8
           | Length Float Unit
  deriving (Show, Eq)

data Unit = Px --only Px for now
  deriving (Show, Eq)

type Specificity = (Word,Word,Word)


-- an empty selector
nilS = Simple Nothing Nothing []

parseCSS :: T.Text -> Either ParseError Stylesheet
parseCSS css = case runParser rules nilS "" css of
    Left err -> Left err
    Right rs -> Right (Stylesheet rs)



rules = spaces >> manyTill (rule <* spaces) eof


rule = do
    s <- selectors
    d <- declarations
    return $ Rule s d

-- CHECKME: does the applicative version of rule follow parsing order?
-- rule = liftM2 Rule selectors declarations
-- rule = liftA2 Rule selectors declarations
-- rule = Rule <$> selectors <*> declarations

selectors = (sortBy comp) <$> sepEndBy1 selector comma <*
                             (spaces *> char '{' <?> "another selector")
  where comma = between spaces spaces (char ',')
        comp a b = spec a `compare` spec b


spec :: Selector -> Specificity
spec (Simple name id cls) = (maybeLen id, fromIntegral $ length cls, maybeLen name)
  where maybeLen = fromIntegral . maybe 0 T.length

-- parse a simple selector
selector = do
    putState nilS
    manyUnless (id <|> cls <|> univ <|> name) eof
    getState


id = do
    char '#'
    i <- identifier
    modifyState (\(Simple n _ cs) -> Simple n (Just i) cs)

cls = do
    char '.'
    c <- identifier
    modifyState (\(Simple n i cs) -> Simple n i (cs++[c]))

univ = char '*' >> return ()

name = do
    n' <- validId
    n  <- identifier
    let nm = n' `T.cons` n
    modifyState (\(Simple _ i cs) -> Simple (Just nm) i cs)


declarations = do
    char '{'
    spaces *> manyTill (declaration <* spaces) (char '}')

declaration = do
    n <- identifier
    spaces
    char ':'
    spaces
    v <- value
    spaces
    char ';'
    return $ Declaration n v


value = len <|> color <|> keyword

len = do
    f <- float
    u <- unit
    return $ Length f u

float :: Stream s m Char => ParsecT s u m Float
float = (fst . head . readFloat) <$> many (digit <|> (char '.'))

unit = do
    char 'p' <|> char 'P'
    char 'x' <|> char 'X'
    return Px


color = do
    char '#'
    cs <- count 3 (count 2 hexDigit)
    let [r,g,b] = map (fst . head . readHex) cs
    return $ Color r g b 255

keyword = Keyword <$> identifier

identifier = T.pack <$> many validId

validId = alphaNum <|> char '-' <|> char '_'

-- manyTill, but the terminal parser is optional
-- manyUnless p end = many (p <* notFollowedBy end)
manyUnless p end = many ((notFollowedBy end) *> p)