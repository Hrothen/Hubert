{-# LANGUAGE OverloadedStrings #-}
module Painting
    ( Canvas (..)
    , newCanvas
    , paint
    ) where

import Data.Monoid ((<>),mempty)
import Data.Word

import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Text as T

import Control.Lens

import Dom
import Layout
import Style
import CSS (Value(ColorValue), Color(..))

type DisplayList = V.Vector DisplayCommand

data DisplayCommand = SolidColor Color Rect

data Canvas = Canvas { pixels :: V.Vector Color
                     , wdth   :: Word
                     , hght   :: Word }


paint :: LayoutBox -> Rect -> Canvas
paint root bounds = let dlist  = buildDisplayList root
                        canvas = newCanvas w h
                        w = fromInteger . floor $ bounds^.width
                        h = fromInteger . floor $ bounds^.height
                    in F.foldl' paintItem canvas dlist


buildDisplayList :: LayoutBox -> DisplayList
buildDisplayList = F.foldMap renderLayoutBox

renderLayoutBox :: (Dimensions,BoxType) -> DisplayList
renderLayoutBox box = renderBackgroud box <> renderBorders box

renderBackgroud :: (Dimensions,BoxType) -> DisplayList
renderBackgroud (dim,ty) = maybe mempty
    (return . flip SolidColor (borderBox dim)) (getColor ty "background")

getColor :: BoxType -> T.Text -> Maybe Color
getColor (BlockNode style) name  = getColor' style name
getColor (InlineNode style) name = getColor' style name
getColor AnonymousBlock _        = Nothing

getColor' style name = case value (NTree style []) name of
                         Just (ColorValue (Color r g b a)) -> Just (Color r g b a)
                         _                    -> Nothing

renderBorders :: (Dimensions,BoxType) -> DisplayList
renderBorders (dim,ty) = maybe mempty renderBorders' (getColor ty "border-color")
  where
    renderBorders' color = V.fromList $ map (SolidColor color) [l, r, t, b]
    bbox = borderBox dim
    bdr  = dim^.border
    
    l = bbox & width.~ bdr^.left
    
    r = bbox & x+~ bbox^.width - bdr^.right
             & width.~ bdr^.right
    
    t = bbox & height.~ bdr^.top
    
    b = bbox & y+~ bbox^.height - bdr^.bottom
             & height.~ bdr^.bottom

newCanvas :: Word -> Word -> Canvas
newCanvas w h = let white = Color 255 255 255 255 in
                Canvas (V.replicate (fromIntegral(w * h)) white) w h

paintItem :: Canvas -> DisplayCommand -> Canvas
paintItem cs (SolidColor color rect) = updateChunk cs (x0,x1) (y0,y1) color
  where
    x0 = clampInt 0 (w-1) (rect^.x)
    y0 = clampInt 0 (h-1) (rect^.y)
    x1 = clampInt 0 (w-1) (rect^.x + rect^.width - 1)
    y1 = clampInt 0 (h-1) (rect^.y + rect^.height - 1)
    w = asFloat $ wdth cs
    h = asFloat $ hght cs
    asFloat = fromInteger . toInteger



-- this probably modifies the pixel vector in-place, if I'm reading the
-- Data.Vector source correctly
updateChunk :: Canvas -> (Integer,Integer) -> (Integer,Integer) -> Color -> Canvas
updateChunk cs (x0,x1) (y0,y1) c = let pxs = V.update (pixels cs) chunk in
                                   cs{ pixels = pxs}
  where
    chunk = V.map (\a->(fromIntegral a,c)) indicies
    indicies = V.fromList [ y * toInteger (wdth cs) + x | x <- [x0..x1], y <- [y0..y1] ]


clampInt :: Float -> Float -> Float -> Integer
clampInt f c = floor . min c . max f
