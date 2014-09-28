{-#LANGUAGE BangPatterns#-}
module Layout where

import Dom
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
    Block       -> Right blt root
    Inline      -> Right blt root
    DisplayNone -> Left "error: root node has display:none"
  where
    blt rt@(NTree nd cs) = NTree (defaultDim, n) ns
      where 
        (!n, !ns) = case display rt of
            Block  -> (BlockNode  nd, anonify ns')
            Inline -> (InlineNode nd, ns')
            -- won't ever hit DisplayNone, it's filtered out
        
        anonify = concatMap foo . groupBy isInline
        
        foo x = if isInline $ head x then [NTree AnonymousBlock x] else x

        isInline InlineNode{} = True
        isInline _            = False
        
        ns' = map blt $ filter ((/=DisplayNone) . display) cs


layout :: LayoutBox -> Dimensions -> LayoutBox
layout l@(NTree (_,box)) contBlock = case box of
    BlockNode  _   -> layoutBlock l contBlock
    InlineNode _   -> undefined
    AnonymousBlock -> undefined


calcWidth :: LayoutBox -> Dimensions -> LayoutBox
calcWidth root@(NTree (dim,x) y) contBlock = let
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
    total = sum $ map toPix (w:vals)
    underflow = (width contBlock) - total

    ([ml'',mr''],vals') = splitAt 2 vals
    (w',ml',mr') = checkUnderflow w $ checkAutoMargins (ml'',mr'')

    checkAutoMargins margins
        | w /= auto && total > (width contBlock) = case margins of
            (auto,auto) -> (zero,zero)
            (auto,x)    -> (zero,x)
            (x,auto)    -> (x,zero)
            (x,y)       -> (x,y)
        | otherwise = margins

    checkUnderflow w (mlf,mrt) = case (w == auto, mlf == auto, mrt == auto) of
        (False,False,False) -> (w , mlf, Length (toPix mrt + underflow) Px)
        (False,False,True)  -> (w , mlf, Length underflow Px)
        (False,True,False)  -> (w , Length underflow Px    , mrt)
        (False,True,True)   -> (w , Length (underflow/2) Px, Length (underflow/2) Px)
        (True,_,_)          ->
            let l = if mlf == auto then zero else mlf
                r = if mrt == auto then zero else mrt
             in if underflow >= 0  then (Length underflow px,l,r)
                                   else (zero,l,Length (toPix r + underflow) Px)

    [w'',ml,mr,blw,brw,plf,prt] = map toPix (w':ml':mr':vals')


    updateDim d = let pad = padding d
                      mar = margin d
                      bor = border d
                   in d{ width = w''
                       , padding = pad{ left = plf, right = plr}
                       , boarder = bor{ left = blf, right = blr}
                       , margin  = mar{ left = ml,  right = mr} }

    in in NTree (updateDim dim,x) y


calcPosition :: LayoutBox -> Dimensions -> Dimensions
calcPosition root@(NTree (dim,_)_) contBlock = let
    style = getStyledElem root
    zero = Length 0 Px

    vals = map (\a -> lookup style a zero) [
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
--     total = sum $ map toPix [width'',mlf'',mrt'',blw',brw',plf',prt']
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
--         (False,False,False) -> (width'',mlf'', Length (toPix mrt'' + underflow) Px)
--         (False,False,True)  -> (width'',mlf'', Length underflow Px)
--         (False,True,False)  -> (width'', Length underflow Px, mrt'')
--         (False,True,True)   -> (width'', Length (underflow/2) Px, Length (underflow/2) Px)
--         (True,_,_)          -> let ml = if mlf'' == auto then zero else mlf''
--                                    mr = if mlr'' == auto then zero else mlr''
--                                 in if underflow >= 0 then (Length underflow px,ml,mr)
--                                    else (zero,ml,Length (toPix mr + underflow) Px)
--     [width,mlf,mrt,blw,brw,plf,prt] = map toPix [width',mlf'',mrt'',blw,brw,plf,prt]
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

getStyledElem :: LayoutBox -> Maybe StyledElement
getStyledElem (NTree (_,box) _) = case box of
    BlockNode  s   -> Just s
    InlineNode s   -> Just s
    AnonymousBlock -> Nothing -- this is actually an error