module Style where

import Data.Maybe (mapMaybe)
import Data.List (foldl')

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Dom
import CSS

type PropertyMap = HM.Map T.Text Value

-- instead of building a tree with references to the DOM, we'll
-- just augment the DOM tree with PropertyMaps
type StyledNode = NTree (NodeType,PropertyMap)

type MatchedRule = (Specificity, Rule)

-- check if a selector matches an element
matches :: ElementData -> Selector -> Bool
matches e sl@(Simple _ _ _) = matchSimple e sl


-- matchSimple returns False if any selector field that exists
-- does not match the given element
matchSimple ElementData -> Selector -> Bool
matchSimple e@(ElementData nm _) (Simple n i c) = 
  let x = fmap (==nm) n
      y = fmap (==(findId e)) i
      z = if not $ null c then any (not . member (classes e)) c else True
  in case (x,y,z) of
      (Nothing, Nothing, b3) -> b3
      (Nothing, Just b2, b3) -> b2 && b3
      (Just b1, Nothing, b3) -> b1 && b3
      (Just b1, Just b2, b3) -> b1 && b2 && b3


-- find the first rule that matches the given element
matchRule :: ElementData -> Rule -> Maybe MatchedRule
matchRule e r@(Rule selectors _) = do
    s <- find (matches e) selectors
    return (spec s, r)


-- get all of the rules from a stylesheet that match the given element
matchingRules :: ElementData -> Stylesheet -> [MatchedRule]
matchingRules e (Stylesheet rules) = mapMaybe (matchRule e) rules


-- Build a map of all the properties attached to an Element
specifiedValues :: ElementData -> Stylesheet -> PropertyMap
-- specifiedValues e s = foldl' foo HM.empty rules
--   where
--     rules = sortBy (\a b -> fst a `compare` fst b) $ matchingRules e s
--     foo vs (_,Declaration n v) = HM.insert n v vs 


specifiedValues = HM.fromList . map (\(_,Declaration n v)->(n,v)) rules
  where
    rules = sortBy (\a b->fst a `compare` fst b) . matchingRules


-- traverse the DOM, attaching PropertyMaps to each Node to
-- create a styled tree
styleTree :: Node -> Stylesheet -> StyledNode
styleTree root stylesheet = fmap style root
  where
    style e@(Element e') = (e, specifiedValues e')
    style t@(Text _)     = (t, HM.empty)