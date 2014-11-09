{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Dom where

import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Foldable

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Data.HashSet


data NTree a = NTree a [NTree a]
  deriving (Show,Eq)

instance Functor NTree where
    fmap f (NTree n ns) = NTree (f n) $ fmap (fmap f) ns

instance Foldable NTree where
    foldMap f (NTree n []) = f n
    foldMap f (NTree n ns) = f n <> foldMap (foldMap f) ns

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
elem name atts = NTree (Element (ElementData name atts))

findAttr :: ElementData -> T.Text -> Maybe T.Text
findAttr (ElementData _ m) k = HM.lookup k m

findID :: ElementData -> Maybe T.Text
findID = flip findAttr "id"

classes :: ElementData -> HashSet T.Text
classes = maybe empty (fromList . T.split (==' ')) . flip findAttr "class"