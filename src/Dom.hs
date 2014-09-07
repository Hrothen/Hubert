{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Dom where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Test.QuickCheck
import Test.QuickCheck.Instances
import Control.Monad (liftM, liftM3)

data NTree a = NTree a [NTree a]
  deriving (Show)


-- data specific to each node type
data NodeType = Text T.Text 
              | Element ElementData
  deriving (Show)

type Node = NTree NodeType


type AttrMap = HM.HashMap T.Text T.Text

data ElementData = ElementData T.Text AttrMap
  deriving (Show)


text :: T.Text -> Node
text = flip NTree [] . Text

elem :: T.Text -> AttrMap -> [Node] -> Node
elem name atts cs = NTree (Element (ElementData name atts)) cs


-------------------------------- USED FOR TESTS -------------------------------

-- print a node as an html string
toHtml :: Node -> T.Text
toHtml (NTree (Text t) cs) = t `T.append` (T.concat $ map toHtml cs)
toHtml (NTree (Element (ElementData t m)) cs) =
    tag `T.append` attrs `T.append` ('>' `T.cons` children `T.append` tag')
  where
    tag = '<' `T.cons` t `T.snoc` ' '
    attrs = T.cons ' ' $ T.intercalate " " $ map (\(x,y)->x`T.append`"="`T.append`y) $ HM.toList m
    children = T.concat $ map toHtml cs
    tag' = '<' `T.cons` t `T.snoc` '>'

instance Arbitrary (NTree NodeType) where
    arbitrary = sized tree'
      where
        tree' 0 =
            oneof [liftM text arbitrary,
                   liftM3 Dom.elem arbitrary arbitrary (vector 0)]
        tree' n | n > 0 =
            oneof [liftM text arbitrary,
                   liftM3 Dom.elem arbitrary arbitrary subtree]
          where subtree = listOf (tree' (n-1)) -- n decides depth of tree
    -- coarbitrary (NTree (Text t) _) = variant 0 . coarbitrary t
    -- coarbitrary (NTree (Element (ElementData t m)) cs) =
    --     variant 1 . coarbitrary t . coarbitrary m