module Layout where

import Dom

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

data BoxType = BlockNode StyledNode | InlineNode StyledNode | AnonymousBlock