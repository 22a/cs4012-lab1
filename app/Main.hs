{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shapes

import Codec.Binary.Base64.String
import qualified Data.Text.Lazy as L

-- Blaze SVG Stuff
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

-- Scotty Stuff
import Web.Scotty
import Data.Monoid (mconcat)


main = scotty 3000 $ do
    get "/" $ file "index.html"

    get "/:enc" $ do
        encStr <- param "enc"
        let d = base64ToDrawing encStr
        let s = renderSvg (drawingToSvg d)
        html (L.pack s)

base64ToDrawing :: String -> Drawing
base64ToDrawing s = read (decode s)

drawingToSvg :: Drawing -> S.Svg
drawingToSvg shapes = baseSvgDoc $ mapM_ shapeToSvgElement shapes

baseSvgDoc :: S.Svg -> S.Svg
baseSvgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" ! A.viewbox "0 0 50 50"

shapeToSvgElement :: (Transform, Shape, Style) -> S.Svg
shapeToSvgElement (ts,sh,(sw,sc,fc)) = shape ! strokeWidth ! stroke ! fill ! trans
  where shape = shapeToSvgShape sh
        strokeWidth = A.strokeWidth (S.toValue sw)
        stroke = A.stroke (S.toValue sc)
        fill = A.fill (S.toValue fc)
        trans = A.transform (S.matrix a b c d e f)
        (a,b,c,d,e,f) = getMatTransVals m
        m = transform ts

shapeToSvgShape :: Shape -> S.Svg
shapeToSvgShape shape
  | shape == empty = S.rect
  | shape == circle = S.circle ! A.cx "1" ! A.cy "1" ! A.r "5"
  | shape == square = S.rect ! A.width "10" ! A.height "10"
