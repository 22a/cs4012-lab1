{-# LANGUAGE OverloadedStrings #-}
module Main where

import Shapes

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
    get "/" $ do
      let a = renderSvg svgDoc
      html (L.pack a)
    get "/:word" $ do
        -- beam <- param "word"
        let a = renderSvg (drawingToSvg (((scale 2 2) <+> (translate 2 3)), square,(1,"#000000","#ff0000") ))
        html (L.pack a)

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" ! A.viewbox "0 0 50 50" $ do
    S.rect ! A.width "10" ! A.height "10" ! A.fill "#ff0000" ! A.rotate "30 5 5"
    S.rect ! A.width "5" ! A.height "5" ! A.fill "#0000ff"
    S.rect ! A.width "1" ! A.height "1" ! A.fill "#00ff00"

drawingToSvg :: (Transform, Shape, Style) -> S.Svg
drawingToSvg (ts,shape,(sw,sc,fc)) = S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" ! A.viewbox "0 0 50 50" $ do
  let m = transform ts
  S.rect ! A.width "10" ! A.height "10" ! A.strokeWidth (S.toValue sw) ! A.stroke (S.toValue sc) ! A.fill (S.toValue fc) ! A.transform (S.matrix (getA m) (getB m) (getC m) (getD m) (getE m) (getF m))
