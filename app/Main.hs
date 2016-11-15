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

    get "/:end" $ do
        encStr <- param "enc"
        let d = base64ToDrawing encStr
        let s = renderSvg (drawingToSvg d)
        html (L.pack s)

base64ToDrawing :: String -> Drawing
base64ToDrawing s = read (decode s)

drawingToSvg :: Drawing -> S.Svg
drawingToSvg shapes = S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" ! A.viewbox "0 0 50 50" $ mapM_ shapeToSvgShape shapes

shapeToSvgShape :: (Transform, Shape, Style) -> S.Svg
shapeToSvgShape (ts,shape,(sw,sc,fc)) = sh ! A.strokeWidth (S.toValue sw) ! A.stroke (S.toValue sc) ! A.fill (S.toValue fc) ! A.transform (S.matrix (getA m) (getB m) (getC m) (getD m) (getE m) (getF m))
  where m = transform ts
        sh
          | shape == circle = S.circle ! A.cx "1" ! A.cy "1" ! A.r "2"
          | shape == square = S.rect ! A.width "10" ! A.height "10"

