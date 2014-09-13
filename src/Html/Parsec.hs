{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module HTML.Parsec
    ( parseHtml,
      parseText,
      parseElement
    ) where

import Control.Monad (liftM)
import Control.Applicative ((<*))

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import qualified Data.HashMap.Strict as HM

import Dom

parseHtml :: T.Text -> Either ParseError Node
parseHtml s = case parse parseNodes "" s of
              Left err -> Left err
              Right nodes -> Right $
                if length nodes == 1
                then head nodes
                else Dom.elem "html" HM.empty nodes


parseNodes = spaces >> manyTill (spacesAfter parseNode) end
  where
    end = eof <|> (try (string "</") >> return ())


parseNode = parseElement <|> parseText

parseText = liftM (Dom.text . T.pack) $ many (noneOf "<")

parseElement = do
    -- opening tag
    (tag, attrs) <- between (char '<') (char '>') tagData
    -- contents
    children <- parseNodes
    -- closing tag
    string $ tag ++ ">" -- "</" is consumed by parseNodes, maybe bad form?
    return $ Dom.elem (T.pack tag) attrs children


-- parseChildren = spaces >> manyTill parseChild end
--   where
--     end = eof <|> (try (string "</") >> return ())
-- 
--     parseChild = spacesAfter parseNode


tagData = do
    t <- tagName
    attrs <- attributes
    return (t,attrs)

tagName = many1 alphaNum

--this is safe because attribute will fail without consuming on '>''
attributes = liftM HM.fromList $ spaces >> many (spacesAfter attribute)

attribute = do
    name <- tagName
    char '='
    open <- char '\"' <|> char '\''
    value <- manyTill anyChar (try $ char open)
    return (T.pack name, T.pack value)


-- run parser p and then strip the trailing spaces, returning the result of p.
spacesAfter p = p <* spaces
