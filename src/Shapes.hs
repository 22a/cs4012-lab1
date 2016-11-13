module Shapes() where

import Data.Matrix

-- Utilities

pos :: Double -> Double -> Matrix Double
pos cx cy = fromLists [ [cx], [cy], [1] ]

trans_mat :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix Double
trans_mat a b c d e f = fromLists [ [a,c,e],
                                    [b,d,f],
                                    [0,0,1] ]

do_trans :: Matrix Double -> Matrix Double -> Matrix Double
do_trans cur_pos trans = multStd cur_pos trans

-- Styles

data Style = Style Double String String
           deriving Show

-- Shapes

data Shape = Empty
           | Circle
           | Square
           deriving Show

empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

-- Transformations

data Transform = Identity
           | Translate Double Double
           | Scale Double Double
           | Rotate Double
           | Compose Transform Transform
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Matrix Double -> Matrix Double
transform Identity x = id x
transform (Translate tx ty) m = do_trans m (trans_mat 1 0 0 1 tx ty)
transform (Scale sx sy) m = do_trans m (trans_mat sx 0 0 sy 0 0)
transform (Rotate a) m = do_trans m (trans_mat (cos a) (sin a) (-sin a) (cos a) 0 0)
transform (Compose t1 t2) m = transform t2 $ transform t1 m

-- Drawings

type Drawing = [(Transform,Shape,Style)]

-- interpretation function for drawings
