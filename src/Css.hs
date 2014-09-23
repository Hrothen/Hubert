{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module CSS
    ( Stylesheet(..)
    , Rule(..)
    , Selector(..)
    , Declaration(..)
    , Value(..)
    , Unit(..)
    , Specificity(..)
    , parseCSS
    , selectors
    , declarations
    , spec
    ) where

import Prelude hiding (id)

import Data.Word (Word(..), Word8(..))
import Data.List (sortBy)
import Data.Maybe (maybe)
import Numeric (readFloat, readHex)
import Control.Monad (void)
import Control.Applicative ((<*), (*>), (<$>), (<*>))

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

-- compute the specificity of a Selector
spec :: Selector -> Specificity
spec (Simple name id cls) = (maybeLen id, fromIntegral $ length cls, maybeLen name)
  where maybeLen = fromIntegral . maybe 0 T.length


-- an empty selector
nilS = Simple Nothing Nothing []


-- parse an entire CSS document into a Stylesheet
parseCSS :: T.Text -> Either ParseError Stylesheet
parseCSS css = case runParser rules nilS "" css of
    Left err -> Left err
    Right rs -> Right (Stylesheet rs)



rules = spaces >> manyTill (rule <* spaces) eof


-- rule = do
--     s <- selectors
--     d <- declarations
--     return $ Rule s d

rule = Rule <$> selectors <*> declarations


selectors = sortBy comp <$> sepBy1 (selector <* spaces) comma
  where comma = char ',' <* spaces
        comp a b = spec a `compare` spec b


-- parse a simple selector
selector = do
    putState nilS
    manyUnless (id <|> cls <|> univ <|> name) eof
    getState


-- selector id
id = do
    char '#'
    i <- identifier
    modifyState (\(Simple n _ cs) -> Simple n (Just i) cs)

-- selector class
cls = do
    char '.'
    c <- identifier
    modifyState (\(Simple n i cs) -> Simple n i (cs++[c]))

-- universal selector
univ = void (char '*')

-- selector name
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
    spaces >> char ':' >> spaces
    v <- value
    spaces >> char ';'
    return $ Declaration n v


value = len <|> color <|> keyword

-- len = do
--     f <- float
--     u <- unit
--     return $ Length f u
len = Length <$> float <*> unit

-- parse a floating point number
float :: Stream s m Char => ParsecT s u m Float
float = (fst . head . readFloat) <$> many (digit <|> char '.')

-- parse the unit type in a Value
-- currently only Px is supported
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
manyUnless p end = many (notFollowedBy end *> p)
