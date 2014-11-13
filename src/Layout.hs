{-#LANGUAGE BangPatterns, OverloadedStrings#-}
module Layout where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.List (foldl', groupBy)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T

import Dom
import CSS
import Style

data Rect = Rect { x      :: Float
                 , y      :: Float
                 , width  :: Float
                 , height :: Float }

data Dimensions = Dimensions { content :: Rect
                             , padding :: EdgeSize
                             , border  :: EdgeSize
                             , margin  :: EdgeSize }

data EdgeSize = EdgeSize { left   :: Float
                         , right  :: Float
                         , top    :: Float
                         , bottom :: Float }

type LayoutBox = NTree (Dimensions,BoxType)

type StyledElement = (NodeType,PropertyMap)

data BoxType = BlockNode StyledElement | InlineNode StyledElement | AnonymousBlock

emptyEdge = EdgeSize 0 0 0 0
emptyRect = Rect 0 0 0 0

defaultDim = Dimensions emptyRect emptyEdge emptyEdge emptyEdge

-- walk the style tree, building a layout tree as we go
-- FIXME: I suspect this function leaks space
buildLayoutTree :: StyledNode -> Either T.Text LayoutBox
buildLayoutTree root = case display root of
    Block       -> Right $ addDim <$> blt root
    Inline      -> Right $ addDim <$> blt root
    DisplayNone -> Left "error: root node has display:none"
  where
    addDim x = (defaultDim,x)

    blt rt@(NTree nd cs) = NTree n ns
      where 
        (!n, !ns) = case display rt of
            Block  -> (BlockNode  nd, anonify ns')
            Inline -> (InlineNode nd, ns')
            -- won't ever hit DisplayNone, it's filtered out
        
        anonify = concatMap mergeInlines . groupBy (\x y -> isInline x && isInline y)
        
        mergeInlines x = if isInline $ head x then [NTree AnonymousBlock x] else x

        isInline (NTree InlineNode{} _) = True
        isInline _                      = False
        
        ns' = map blt $ filter ((/=DisplayNone) . display) cs


-- walk a layout tree, setting the dimensions of each node
layout :: LayoutBox -> Dimensions -> Either T.Text LayoutBox
layout l@(NTree (_,box)_) contBlock = case box of
    BlockNode  _   -> layoutBlock contBlock l
    InlineNode _   -> undefined
    AnonymousBlock -> undefined


layoutBlock dim root = calcWidth dim root >>=
                       calcPosition dim   >>=
                       layoutChildren     >>=  -- you know what? this might leak
                       calcHeight


-- walk a layout tree, computing the width of each box
-- NB: This looks really fugly, I am completely sure that there's a nicer
-- way to write this, but it escapes me at the moment
calcWidth :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcWidth contBlock root@(NTree (dim,x) y) = do
    style <- getStyledElem root
    let
      auto = Keyword "auto"
      zero = Length 0 Px
      w = fromMaybe auto $ value style "width"
      vals = map (\a -> lookup style a zero) [
                    ["margin-left"       , "margin"]
                  , ["margin-right"      , "margin"]
                  , ["border-left-width" , "border-width"]
                  , ["border-right-width", "border-width"]
                  , ["padding-left"      , "padding"]
                  , ["padding-right"     , "padding"] ]
      total = sum $ map toPx (w:vals)
      underflow = width (content contBlock) - total

      ([ml'',mr''],vals') = splitAt 2 vals
      (w',ml',mr') = checkUnderflow w $ checkAutoMargins (ml'',mr'')

      checkAutoMargins (x,y)
          | w /= auto && total > width (content contBlock) = (check x,check y)
          | otherwise = (x,y)
        where check a = if a == auto then zero else a

      checkUnderflow w (mlf,mrt) = case (w == auto, mlf == auto, mrt == auto) of
          (False,False,False) -> (w , mlf, Length (toPx mrt + underflow) Px)
          (False,False,True)  -> (w , mlf, Length underflow Px)
          (False,True,False)  -> (w , Length underflow Px    , mrt)
          (False,True,True)   -> (w , Length (underflow/2) Px, Length (underflow/2) Px)
          (True,_,_)          ->
              let l = if mlf == auto then zero else mlf
                  r = if mrt == auto then zero else mrt
               in if underflow >= 0  then (Length underflow Px,l,r)
                                     else (zero,l,Length (toPx r + underflow) Px)

      [w'',ml,mr,blw,brw,plf,prt] = map toPx (w':ml':mr':vals')


      updateDim d = let pad = padding d
                        mar = margin d
                        bor = border d
                        rec = content d
                     in d{ content = rec{ width = w'' }
                         , padding = pad{ left  = plf, right = prt }
                         , border  = bor{ left  = blw, right = brw }
                         , margin  = mar{ left  = ml,  right = mr } }

    return $ NTree (updateDim dim,x) y


-- walk a layout tree, computing the position of each box
calcPosition :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcPosition contBlock root@(NTree (dim,a)b) = do
    style <- getStyledElem root
    
    let
      zero = Length 0 Px

      vals = map (toPx .  (\a -> lookup style a zero)) [
                    ["margin-top"         , "margin"]
                  , ["margin-bottom"      , "margin"]
                  , ["border-top-width"   , "border-width"]
                  , ["border-bottom-width", "border-width"]
                  , ["padding-top"        , "padding"]
                  , ["padding-bottom"     , "padding"] ]

      updateDim d [mt,mb,bt,bb,pt,pb] =
          let pad  = padding d
              mar  = margin d
              bor  = border d
              brec = content contBlock
              drec = content d
              x' = x brec
                 + left (margin d)
                 + left (border d)
                 + left (padding d)
              y' = y brec + height brec + pt + bt + mt
           in d{ content = drec{ x = x', y = y' }
               , padding = pad{ top = pt, bottom = pb }
               , border  = bor{ top = bt, bottom = bb }
               , margin  = mar{ top = mt, bottom = mb } }

    return $ NTree (updateDim dim vals,a) b

-- recursively lay out the children of a node
layoutChildren (NTree (dim,x) cs) = do
    (dim',cs') <- foldM foo (dim,[]) cs
    return $ NTree (dim',x) cs'

    where
        foo (d,acc) c@(NTree (cdim,_) _) = do
            c' <- layout c d
            let rec = content d
            return (d{ content =
                rec{height = height rec + marginBoxHeight cdim}}, acc ++ [c'])


-- compute the hight of a box
calcHeight :: LayoutBox -> Either T.Text LayoutBox
calcHeight root@(NTree (d,x)y) = do
    s <- getStyledElem root
    let d' = case value s "height" of
             Just (Length h Px)  -> d{content = (content d){height=h}}
             Nothing             -> d
    return $ NTree (d',x) y


marginBoxHeight :: Dimensions -> Float
marginBoxHeight (Dimensions c p b m) = sum [ height c, top p, bottom p
                                                     , top b, bottom b
                                                     , top m, bottom m ]


getStyledElem :: LayoutBox -> Either T.Text StyledNode
getStyledElem (NTree (_,box) _) = case box of
    BlockNode  s   -> Right $ NTree s []
    InlineNode s   -> Right $ NTree s []
    AnonymousBlock -> Left "Error: attempted to access the nonexistant\
                           \ StyleNode of an AnonymousBlock"


-- Rect and Dimensions helpers

expandedBy :: EdgeSize -> Rect -> Rect
expandedBy edge rec = Rect{ x      = x rec - left edge
                          , y      = y rec - top edge
                          , width  = width rec + left edge + right edge
                          , height = height rec + top edge + bottom edge }


paddingBox :: Dimensions -> Rect
paddingBox d = expandedBy (padding d) $ content d

marginBox :: Dimensions -> Rect
marginBox d = expandedBy (margin d) $ content d

borderBox :: Dimensions -> Rect
borderBox d = expandedBy (border d) $ content d