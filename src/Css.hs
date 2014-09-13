module CSS where

import Data.Word(Word8(..))

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

data Stylesheet = Stylesheet [Rule]

data Rule = Rule [Selector] [Declaration]

-- only handle simple selectors for now
data Selector = Simple SimpleSelector

data SimpleSelector = SimpleSelector (Maybe T.Text) (Maybe T.Text) [T.Text]

data Declaration = Declaration T.Text Value

data Value = Keyword T.Text
           | Color Word8 Word8 Word8 Word8
           | Length Float Unit

data Unit = Px --only Px for now

simpleSelector = 