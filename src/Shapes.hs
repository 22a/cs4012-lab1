module Shapes(Drawing, Transform, Shape, Style, empty, circle, square, transform, ident, translate, scale, rotate, (<+>), getA, getB, getC, getD, getE, getF) where

import Data.Matrix

-- Utilities

trans_mat :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix Double
trans_mat a b c d e f = fromLists [ [a,c,e],
                                    [b,d,f],
                                    [0,0,1] ]

mat_mul :: Matrix Double -> Matrix Double -> Matrix Double
mat_mul t1 t2 = multStd t1 t2

-- Styles

type Style = (Double,String,String)

-- Shapes

data Shape = Empty
           | Circle
           | Square
           deriving (Show, Read)

empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

-- Transformations

data Transform = Ident
           | Translate Double Double
           | Scale Double Double
           | Rotate Double
           | Compose Transform Transform
             deriving (Show, Read)

ident = Ident
translate = Translate
scale = Scale
rotate = Rotate
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Matrix Double
transform Ident = (trans_mat 1 0 0 1 0 0)
transform (Translate tx ty) = (trans_mat 1 0 0 1 tx ty)
transform (Scale sx sy) = (trans_mat sx 0 0 sy 0 0)
transform (Rotate a) = (trans_mat (cos a) (sin a) (-sin a) (cos a) 0 0)
transform (Compose t1 t2) = mat_mul (transform t1) (transform t2)

-- Drawings

type Drawing = (Transform,Shape,Style)

-- interpretation function for drawings

getA :: Matrix Double -> Double
getA m = getElem 1 1 m

getB :: Matrix Double -> Double
getB m = getElem 2 1 m

getC :: Matrix Double -> Double
getC m = getElem 1 2 m

getD :: Matrix Double -> Double
getD m = getElem 2 2 m

getE :: Matrix Double -> Double
getE m = getElem 1 3 m

getF :: Matrix Double -> Double
getF m = getElem 2 3 m
