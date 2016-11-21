module Shapes(Drawing,
             Transform,
             Shape,
             Style,
             transform,
             empty,
             circle,
             square,
             getMatTransVals) where

import Data.Matrix

-- Utilities
trans_to_mat :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix Double
trans_to_mat a b c d e f = fromLists [ [a,c,e], [b,d,f], [0,0,1] ]

compose_trans :: Matrix Double -> Matrix Double -> Matrix Double
compose_trans t1 t2 = multStd t1 t2

deg_to_rad :: Double -> Double
deg_to_rad deg = deg * (pi / 180)

getMatTransVals :: Matrix Double -> (Double,Double,Double,Double,Double,Double)
getMatTransVals m = (getElem 1 1 m, getElem 2 1 m, getElem 1 2 m,
                     getElem 2 2 m, getElem 3 1 m, getElem 2 3 m)

-- Styles
type StrokeWidth = Double
type StrokeColour = String
type FillColour = String

type Style = (StrokeWidth,StrokeColour,FillColour)


-- Shapes
data Shape = Empty
           | Circle
           | Square
           deriving (Show, Read, Eq)

empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

-- Transformations
data Transform = Ident
           | Translate Double Double
           | Scale Double Double
           | Rotate Double
           | SkewX Double
           | SkewY Double
           | Transform :> Transform
             deriving (Show, Read)

transform :: Transform -> Matrix Double
transform Ident = (trans_to_mat 1 0 0 1 0 0)
transform (Translate tx ty) = (trans_to_mat 1 0 0 1 tx ty)
transform (Scale sx sy) = (trans_to_mat sx 0 0 sy 0 0)
transform (SkewX an) = (trans_to_mat 1 0 (tan a) 1 0 0)
  where a = deg_to_rad an
transform (SkewY an) = (trans_to_mat 1 (tan a) 0 1 0 0)
  where a = deg_to_rad an
transform (Rotate an) = (trans_to_mat (cos a) (sin a) (-sin a) (cos a) 0 0)
  where a = deg_to_rad an
transform (t1 :> t2) = compose_trans (transform t1) (transform t2)

-- Drawings
type Drawing = [(Transform,Shape,Style)]
