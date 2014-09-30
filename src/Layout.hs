{-#LANGUAGE BangPatterns, OverloadedStrings#-}
module Layout where

import Prelude hiding (lookup)
import Data.List (foldl', groupBy)

import qualified Data.Text as T

import Dom
import CSS
import Style

data Dimensions = Dimensions { x       :: Float
                             , y       :: Float
                             , width   :: Float
                             , height  :: Float
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

defaultDim = Dimensions 0 0 0 0 emptyEdge emptyEdge emptyEdge

-- walk the style tree, building a layout tree as we go
-- FIXME: I suspect this function leaks space
buildLayoutTree :: StyledNode -> Either T.Text LayoutBox
buildLayoutTree root = case display root of
    Block       -> Right $ blt root
    Inline      -> Right $ blt root
    DisplayNone -> Left "error: root node has display:none"
  where
    blt rt@(NTree nd cs) = NTree (defaultDim, n) ns
      where 
        (!n, !ns) = case display rt of
            Block  -> (BlockNode  nd, anonify ns')
            Inline -> (InlineNode nd, ns')
            -- won't ever hit DisplayNone, it's filtered out
        
        anonify = concatMap foo . groupBy (\x y -> isInline x && isInline y)
        
        foo x = if isInline $ head x then [NTree (defaultDim, AnonymousBlock) x] else x

        isInline (NTree (_,InlineNode{}) _) = True
        isInline _                          = False
        
        ns' = map blt $ filter ((/=DisplayNone) . display) cs


layout :: LayoutBox -> Dimensions -> LayoutBox
layout l@(NTree (_,box)_) contBlock = case box of
    BlockNode  _   -> layoutBlock contBlock l
    InlineNode _   -> undefined
    AnonymousBlock -> undefined


layoutBlock :: Dimensions -> LayoutBox -> LayoutBox
layoutBlock dim = calcHeight . layoutChildren
                . calcPosition dim . calcWidth dim

calcWidth :: Dimensions -> LayoutBox -> LayoutBox
calcWidth contBlock root@(NTree (dim,x) y) = let
    style = getStyledElem root
    auto = Keyword "auto"
    zero = Length 0 Px
    w = maybe auto id $ value style "width"
    vals = map (\a -> lookup style a zero) [
                  ["margin-left"       , "margin"]
                , ["margin-right"      , "margin"]
                , ["border-left-width" , "border-width"]
                , ["border-right-width", "border-width"]
                , ["padding-left"      , "padding"]
                , ["padding-right"     , "padding"] ]
    total = sum $ map toPx (w:vals)
    underflow = (width contBlock) - total

    ([ml'',mr''],vals') = splitAt 2 vals
    (w',ml',mr') = checkUnderflow w $ checkAutoMargins (ml'',mr'')

    checkAutoMargins (x,y)
        | w /= auto && total > (width contBlock) = (check x,check y)
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
                   in d{ width = w''
                       , padding = pad{ left = plf, right = prt}
                       , border  = bor{ left = blw, right = brw}
                       , margin  = mar{ left = ml,  right = mr} }

    in NTree (updateDim dim,x) y


calcPosition :: Dimensions -> LayoutBox -> LayoutBox
calcPosition contBlock root@(NTree (dim,a)b) = let
    style = getStyledElem root
    zero = Length 0 Px

    vals = map (toPx .  (\a -> lookup style a zero)) [
                  ["margin-top"         , "margin"]
                , ["margin-bottom"      , "margin"]
                , ["border-top-width"   , "border-width"]
                , ["border-bottom-width", "border-width"]
                , ["padding-top"        , "padding"]
                , ["padding-bottom"     , "padding"] ]

    updateDim d [mt,mb,bt,bb,pt,pb] =
        let pad = padding d
            mar = margin d
            bor = border d
            x' = (x contBlock)
               + (left $ margin d)
               + (left $ border d)
               + (left $ padding d)
            y' = (y contBlock) + (height contBlock) + pt + bt + mt
         in d{ x = x'
             , y = y'
             , padding = pad{ top = pt, bottom = pb }
             , border  = bor{ top = bt, bottom = bb }
             , margin  = mar{ top = mt, bottom = mb } }

    in NTree (updateDim dim vals,a) b


layoutChildren :: LayoutBox -> LayoutBox
layoutChildren (NTree (dim,x) cs) = NTree (dim',x) cs'
  where
    (dim',cs') = foldl' foo (dim,[]) cs
    foo (d,acc) c@(NTree (cdim,_) _) = let c' = layout c d in
        (d{height = height d + marginBoxHeight cdim}, acc ++ [c'])


calcHeight :: LayoutBox -> LayoutBox
calcHeight root@(NTree (d,x)y) = let
    d' = case value (getStyledElem root) "height" of
        Just (Length h Px)  -> d{height=h}
        Nothing             -> d
    in NTree (d',x) y


marginBoxHeight :: Dimensions -> Float
marginBoxHeight (Dimensions _ _ _ h p b m) = sum [ h, top p, bottom p
                                                 , top b, bottom b
                                                 , top m, bottom m ]

-- calcWidth :: LayoutBox -> Dimensions -> LayoutBox
-- calcWidth root@(NTree (dim,x) y) contBlock = let
--     style = getStyledElem root
--     auto = Keyword "auto"
--     zero = Length 0 Px
--     width'' = maybe auto id $ value style "width"
--     mlf''' = lookup style ["margin-left"       , "margin"]       zero
--     mrt''' = lookup style ["margin-right"      , "margin"]       zero
--     blw' = lookup style ["border-left-width" , "border-width"] zero
--     brw' = lookup style ["border-right-width", "border-width"] zero
--     plf' = lookup style ["padding-left"      , "padding"]      zero
--     prt' = lookup style ["padding-right"     , "padding"]      zero
--     total = sum $ map toPx [width'',mlf'',mrt'',blw',brw',plf',prt']
--     (mlf'',mrt'') = checkAutoMargins
--     underflow = (width contBlock) - total
-- 
--     checkAutoMargins margins
--         | width'' /= auto && total > (width contBlock) = case margins of
--             (auto,auto) -> (zero,zero)
--             (auto,x)    -> (zero,x)
--             (x,auto)    -> (x,zero)
--             (x,y)       -> (x,y)
--         | otherwise = margins
-- 
--     (width',mlf',mrt') =case (width'' == auto, mlf'' == auto, mrt'' == auto) of
--         (False,False,False) -> (width'',mlf'', Length (toPx mrt'' + underflow) Px)
--         (False,False,True)  -> (width'',mlf'', Length underflow Px)
--         (False,True,False)  -> (width'', Length underflow Px, mrt'')
--         (False,True,True)   -> (width'', Length (underflow/2) Px, Length (underflow/2) Px)
--         (True,_,_)          -> let ml = if mlf'' == auto then zero else mlf''
--                                    mr = if mlr'' == auto then zero else mlr''
--                                 in if underflow >= 0 then (Length underflow px,ml,mr)
--                                    else (zero,ml,Length (toPx mr + underflow) Px)
--     [width,mlf,mrt,blw,brw,plf,prt] = map toPx [width',mlf'',mrt'',blw,brw,plf,prt]
-- 
--     updateDim (Dimensions d1 d2 d3 d4 d5 d6 d7) =
--         Dimensions d1
--                    d2
--                    width
--                    d4
--                    d5{left=plf,right=prt}
--                    d6{left=blf,right=brt}
--                    d7{left=mlf,right=mrt}
-- 
--     in NTree (updateDim dim,x) y 

getStyledElem :: LayoutBox -> StyledNode
getStyledElem (NTree (_,box) _) = case box of
    BlockNode  s   -> NTree s []
    InlineNode s   -> NTree s []
    AnonymousBlock -> undefined -- this is actually an error