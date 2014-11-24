{-#LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell#-}
module Layout where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.List (foldl', groupBy)
import Data.Maybe (fromMaybe)

import Control.Lens hiding (children)
import Control.Monad.State.Strict
import Control.Monad.Except

import qualified Data.Text as T

import Dom
import CSS
import Style

data Rect = Rect { _x      :: Float
                 , _y      :: Float
                 , _width  :: Float
                 , _height :: Float }

data Dimensions = Dimensions { _content :: Rect
                             , _padding :: EdgeSize
                             , _border  :: EdgeSize
                             , _margin  :: EdgeSize }

data EdgeSize = EdgeSize { _left   :: Float
                         , _right  :: Float
                         , _top    :: Float
                         , _bottom :: Float }

makeLenses ''Rect
makeLenses ''Dimensions
makeLenses ''EdgeSize

type LayoutBox = NTree (Dimensions,BoxType)

type StyledElement = (NodeType,PropertyMap)

data BoxType = BlockNode StyledElement | InlineNode StyledElement | AnonymousBlock

emptyEdge = EdgeSize 0 0 0 0
emptyRect = Rect 0 0 0 0

defaultDim = Dimensions emptyRect emptyEdge emptyEdge emptyEdge

layoutTree :: StyledNode -> Dimensions -> Either T.Text LayoutBox
layoutTree root contBlock = buildLayoutTree root >>=
                            flip layout (contBlock & content.height.~0)


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
layout l contBlock = case l^.root._2 of
    BlockNode  _   -> layoutBlock contBlock l
    InlineNode _   -> undefined
    AnonymousBlock -> undefined


layoutBlock dim root = calcWidth dim root >>=
                       calcPosition dim   >>=
                       layoutChildren     >>=  -- you know what? this might leak
                       calcHeight


auto = Keyword "auto"
zero = Length 0 Px


calcWidth :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcWidth contBlock rt = do
    style <- getStyledElem rt
    vals  <- lookupSideVals rt
    let w               = fromMaybe auto $ value style "width"
        total           = sum $ map toPx (w:vals)
        underflow       = contBlock^.content.width - total
        (margins,vals') = splitAt 2 vals
        
        (w',ml',mr') = checkUnderflow w underflow $ checkAutoMargins margins w total

        [w'',ml,mr,blw,brw,plf,prt] = map toPx (w':ml':mr':vals')

    return $ rt &~ root._1.content.width.= w''
                &~ root._1.padding.left.= plf &~ root._1.padding.right.= prt
                &~ root._1.border.left.=  blw &~ root._1.border.right.=  brw
                &~ root._1.margin.left.=  ml  &~ root._1.margin.right.=  mr

  where
    checkAutoMargins [x,y] w total
          | w /= auto && total > contBlock^.content.width = (check x,check y)
          | otherwise = (x,y)
        where check a = if a == auto then zero else a

    checkUnderflow w uflow (mlf,mrt) = case (w == auto, mlf == auto, mrt == auto) of
        (False,False,False) -> (w , mlf, Length (toPx mrt + uflow) Px)
        (False,False,True)  -> (w , mlf, Length uflow Px)
        (False,True,False)  -> (w , Length uflow Px    , mrt)
        (False,True,True)   -> (w , Length (uflow/2) Px, Length (uflow/2) Px)
        (True,_,_)          ->
            let l = if mlf == auto then zero else mlf
                r = if mrt == auto then zero else mrt
             in if uflow >= 0  then (Length uflow Px,l,r)
                                   else (zero,l,Length (toPx r + uflow) Px)


-- lookupSideVals :: ErrState [Value]
lookupSideVals :: LayoutBox -> Either T.Text [Value]
lookupSideVals rt = do
    style <- getStyledElem rt
    return $ map (\a -> lookup style a zero)
        [ ["margin-left"       , "margin"]
        , ["margin-right"      , "margin"]
        , ["border-left-width" , "border-width"]
        , ["border-right-width", "border-width"]
        , ["padding-left"      , "padding"]
        , ["padding-right"     , "padding"] ]

lookupVertVals :: LayoutBox -> Either T.Text [Float]
lookupVertVals rt = do
    style <- getStyledElem rt
    return $ map (toPx . (\a -> lookup style a zero)) [
                    ["margin-top"         , "margin"]
                  , ["margin-bottom"      , "margin"]
                  , ["border-top-width"   , "border-width"]
                  , ["border-bottom-width", "border-width"]
                  , ["padding-top"        , "padding"]
                  , ["padding-bottom"     , "padding"] ]


calcPosition :: Dimensions -> LayoutBox -> Either T.Text LayoutBox
calcPosition contBlock rt = do
    [mt,mb,bt,bb,pt,pb] <- lookupVertVals rt
    let d = rt^.root._1
    return $ rt
       &~ root._1.content.x.= contBlock^.content.x
                            + d^.margin.left
                            + d^.border.left
                            + d^.padding.left
       &~ root._1.content.y.= contBlock^.content.y
                            + contBlock^.content.height
                            + pt + bt + mt
       &~ root._1.padding.top.= pt &~ root._1.padding.bottom.= pb
       &~ root._1.border.top.=  bt &~ root._1.border.bottom.= bb
       &~ root._1.margin.top.=  mt &~ root._1.margin.bottom.= mb


layoutChildren :: LayoutBox -> Either T.Text LayoutBox
layoutChildren rt = do
    (dim,cs) <- foldM foo (rt^.root._1,[]) $ rt^.children
    return $ rt &~ root._1.= dim &~ children.= cs
  where
    foo :: (Dimensions,[LayoutBox]) -> LayoutBox -> Either T.Text (Dimensions,[LayoutBox])
    foo (d,acc) c = do
        c' <- layout c d
        return (d & content.height+~ marginBoxHeight (c'^.root._1), acc ++ [c'])


-- compute the hight of a box
calcHeight :: LayoutBox -> Either T.Text LayoutBox
calcHeight rt = do
    s <- getStyledElem rt
    case value s "height" of
        Just (Length h Px)  -> return $ rt & root._1.content.height.~ h
        Nothing             -> return rt


marginBoxHeight :: Dimensions -> Float
marginBoxHeight dim = (marginBox dim)^.height


getStyledElem :: LayoutBox -> Either T.Text StyledNode
getStyledElem rt = case rt^.root._2 of
    BlockNode  s   -> Right $ NTree s []
    InlineNode s   -> Right $ NTree s []
    AnonymousBlock -> Left "Error: attempted to access the nonexistant\
                           \ StyleNode of an AnonymousBlock"


-- Rect and Dimensions helpers

expandedBy :: Rect -> EdgeSize -> Rect
expandedBy rec edge = rec & x -~ edge^.left
                          & y -~ edge^.top
                          & width  +~ (edge^.left + edge^.right)
                          & height +~ (edge^.top + edge^.bottom) 


paddingBox :: Dimensions -> Rect
-- paddingBox d = expandedBy (padding d) $ content d
paddingBox d = (d^.content) `expandedBy` (d^.padding)

marginBox :: Dimensions -> Rect
-- marginBox d = expandedBy (margin d) $ borderBox d
marginBox d = (borderBox d) `expandedBy` (d^.margin)

borderBox :: Dimensions -> Rect
-- borderBox d = expandedBy (border d) $ paddingBox d
borderBox d = (paddingBox d) `expandedBy` (d^.margin)
