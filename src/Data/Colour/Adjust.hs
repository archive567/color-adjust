{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Colour representations and combinations, based on oklab
module Data.Colour.Adjust where

import Chart
import Chart.Data
import Chart.Examples
import Data.Colour
import Data.Functor.Rep
import NumHask.Array.Fixed
import Optics.Core

-- * charts

gradientChart :: Int -> Colour -> Colour -> [Chart]
gradientChart grain c0 c1 =
  (\(r, c) -> RectChart (defaultRectStyle & #color .~ c & #borderSize .~ 0) [r])
    . (\x -> (Rect x (x + d) 0 1, mix x c0 c1))
    <$> grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

gradientChart_ :: Int -> LCHA -> LCHA -> [Chart]
gradientChart_ grain c0 c1 =
  (\(r, c) -> RectChart (defaultRectStyle & #color .~ c & #borderSize .~ 0) [r])
    . (\x -> (Rect x (x + d) 0 1, view lcha2colour' (mixLCHA x c0 c1)))
    <$> grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

gradient :: Maybe Double -> Double -> Double -> Int -> LCHA -> LCHA -> ChartSvg
gradient marker h fa grain ok0 ok1 =
  mempty
    & #svgOptions % #svgHeight
    .~ h
    & #svgOptions % #cssOptions % #shapeRendering
    .~ UseCssCrisp
    & #hudOptions
    .~ ( mempty
           & #chartAspect .~ FixedAspect fa
           & #frames .~ [(20, FrameOptions (Just (border 0.004 white)) 0.1)]
       )
    & #charts
    .~ named "gradient" (gradientChart_ grain ok0 ok1) <> strip
  where
    strip = case marker of
      Nothing -> mempty
      Just marker' ->
        named
          "border"
          [borderStrip 0.02 white (Rect (marker' - 0.02) (marker' + 0.02) (-0.1) 1.1)]

borderStrip :: Double -> Colour -> Rect Double -> Chart
borderStrip w c r = RectChart (defaultRectStyle & #color .~ transparent & #borderSize .~ w & #borderColor .~ c) [r]

wheelPoints :: Int -> Double -> Double -> [(Point Double, Colour)]
wheelPoints grain l maxchroma =
  (\(Point c h) -> (uncurry Point $ view (re xy2ch') (c, h), view lcha2colour' (LCHA' [l, c, h] 1)))
    <$> grid LowerPos (Rect 0 maxchroma 0 360) (Point grain grain)

-- | wheel chart
--
-- > wheel 0.04 100 0.5 0.4
wheel :: Double -> Int -> Double -> Double -> ChartSvg
wheel s grain l maxchroma =
  mempty
    & #hudOptions .~ defaultHudOptions
    & #charts
      .~ named
        "wheel"
        ( ( \(p, c) ->
              GlyphChart
                ( defaultGlyphStyle
                    & #size .~ s
                    & #color .~ c
                    & #borderSize .~ 0
                )
                [p]
          )
            <$> filter (validColour . snd) (wheelPoints grain l maxchroma)
        )

-- | The dotMap
--
-- > dotMap 0.01 20 0.8 0.3
dotMap :: Double -> Int -> Double -> Double -> [Colour] -> ChartSvg
dotMap s grain l maxchroma cs =
  mempty
    & #hudOptions
    .~ defaultHudOptions
    & #charts
    .~ named "dots" (dot_ <$> cs)
    <> named
      "wheel"
      ( ( \(p, c) ->
            GlyphChart
              ( defaultGlyphStyle
                  & #size .~ s
                  & #color .~ c
                  & #borderSize .~ 0
              )
              [p]
        )
          <$> filter (validColour . snd) (wheelPoints grain l maxchroma)
      )

oklch2point_ :: Array '[3] Double -> Point Double
oklch2point_ a = uncurry Point $ view (re xy2ch') (a `index` [1], a `index` [2])

rgb2ch_ :: Colour -> (Point Double, Colour)
rgb2ch_ c = ((\(LCH' lch) -> oklch2point_ lch) . view (re lcha2colour' % lch') $ c, c)

dot_ :: Colour -> Chart
dot_ x = (\(p, c) -> GlyphChart (defaultGlyphStyle & #size .~ 0.08 & #color .~ c & #borderColor .~ Colour 0.5 0.5 0.5 1 & #shape .~ CircleGlyph) [p]) (rgb2ch_ x)
