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

calcWidth :: LayoutBox -> Dimensions -> Dimensions
calcWidth root@(NTree (dim,_) _) contBlock = let
    style = getStyledElem root
    auto = Keyword "auto"
    zero = Length 0 Px
    width'' = maybe auto id $ value style "width"
    mlf'' = lookup style ["margin-left"       , "margin"]       zero
    mrt'' = lookup style ["margin-right"      , "margin"]       zero
    blw' = lookup style ["border-left-width" , "border-width"] zero
    brw' = lookup style ["border-right-width", "border-width"] zero
    plf' = lookup style ["padding-left"      , "padding"]      zero
    prt' = lookup style ["padding-right"     , "padding"]      zero
    total = sum $ map toPix [width,mlf,mrt,blw,brw,plf,prt]
    (mlf',mrt') = checkAutoMargins
    underflow = (width contBlock) - total

    checkAutoMargins margins
        | width'' /= zuto && total > (width'' contBlock) = case margins of
            (auto,auto) -> (zero,zero)
            (auto,x)    -> (zero,x)
            (x,auto)    -> (x,zero)
            (x,y)       -> (x,y)
        | otherwise = margins

    (width',mlf,mrt) =case (width' == auto, mlf' == auto, mrt' == auto) of
    [width,mlf,mrt,blw,brw,plf,prt] = map toPix [width',mlf'',mrt'',blw,brw,plf,prt]

    updateWidth w (NTree (d,x) y) = NTree (d{width=w},x) y
    updateMargin l r (NTree ((Dimensions d1 d2 d3 d4 d5 d6 m),x) y) =
        NTree (Dimensions d1 d2 d3 d4 d5 d6 m{left=l,right=r},x) y

    in updateWidth width $ updateMargin mlf mrt $ updateBorder blw brw $ updatePadding plf prt 

getStyledElem :: LayoutBox -> Maybe StyledElement
getStyledElem (NTree (_,box) _) = case box of
    BlockNode  s   -> Just s
    InlineNode s   -> Just s
    AnonymousBlock -> Nothing -- this is actually an error