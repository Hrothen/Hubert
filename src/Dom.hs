{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Dom where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


data NTree a = NTree a [NTree a]
  deriving (Show,Eq)


-- data specific to each node type
data NodeType = Text T.Text 
              | Element ElementData
  deriving (Show,Eq)

type Node = NTree NodeType


type AttrMap = HM.HashMap T.Text T.Text

data ElementData = ElementData T.Text AttrMap
  deriving (Show,Eq)


text :: T.Text -> Node
text = flip NTree [] . Text

elem :: T.Text -> AttrMap -> [Node] -> Node
elem name atts cs = NTree (Element (ElementData name atts)) cs
