module Dom where

import qualified Data.HashMap.Strict as HM


data NTree a = NTree a [NTree a]
  deriving (Show)

type Node = NTree NodeType

data NodeType = Text String 
              | Element ElementData
  deriving (Show)


type AttrMap = HM.HashMap String String

data ElementData = ElementData String AttrMap
  deriving (Show)


text :: String -> Node
text = flip NTree [] . Text

elem :: String -> AttrMap -> [Node] -> Node
elem name atts cs = NTree (Element (ElementData name atts)) cs