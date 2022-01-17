{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Colour representations and combinations, based on <https://hackage.haskell.org/package/Color>
module Data.Colour.Adjust
  where

import Graphics.Color.Model as M
import Graphics.Color.Adaptation (convert)
import qualified Graphics.Color.Space as S
import System.Random
import System.Random.Stateful
import qualified Graphics.Color.Space.CIE1976.LUV as LUV
import Data.Bool (bool)
import Data.Colour
import Data.Text (Text, pack)
import GHC.Generics hiding (to, Rep)
import NeatInterpolation
import qualified Data.Text as Text
import qualified Graphics.Color.Space.CIE1976.LUV.LCH as LUVLCH
import Chart.Data
import NumHask.Algebra.Metric
import NumHask.Array.Fixed
import Data.Functor.Rep
import GHC.Exts
import Chart
import Optics.Core
import Data.Maybe

newtype Oklch = Oklch' { oklchArray :: Array '[3] Double } deriving (Eq, Show)

pattern Oklch :: Double -> Double -> Double -> Oklch
pattern Oklch l c h <- Oklch' [l,c,h] where
  Oklch l c h = Oklch' [l,c,h]
{-# COMPLETE Oklch #-}

l' :: Lens' Oklch Double
l' = lens (\(Oklch l _ _) -> l) (\(Oklch _ c h) l -> Oklch l c h)

c' :: Lens' Oklch Double
c' = lens (\(Oklch _ c _) -> c) (\(Oklch l _ h) c -> Oklch l c h)

h' :: Lens' Oklch Double
h' = lens (\(Oklch _ _ h) -> h) (\(Oklch l c _) h -> Oklch l c h)

data Oklcha = Oklcha' { lch :: Oklch, lcha :: Double } deriving (Eq, Show)

lch' :: Lens' Oklcha Oklch
lch' = lens (\(Oklcha' lch _) -> lch) (\(Oklcha' _ a) lch -> Oklcha' lch a)

lcha' :: Lens' Oklcha Double
lcha' = lens (\(Oklcha' _ a) -> a) (\(Oklcha' lch _) a -> Oklcha' lch a)

pattern Oklcha :: Double -> Double -> Double -> Double -> Oklcha
pattern Oklcha l c h a <- Oklcha' (Oklch' [l,c,h]) a where
  Oklcha l c h a = Oklcha' (Oklch' [l,c,h]) a
{-# COMPLETE Oklcha #-}

oklch' :: Iso' Oklcha Colour
oklch' = iso toColour_ fromColour_

(!) :: (Representable f, IsList (Rep f)) =>
  f a -> Item (Rep f) -> a
(!) xs i = xs `index` [i]

toColour_ :: Oklcha -> Colour
toColour_ (Oklcha' (Oklch' lch) a) = Colour (rgb!0) (rgb!1) (rgb!2) a
  where
    rgb = oklch2rgb__ lch

fromColour_ :: Colour -> Oklcha
fromColour_ (Colour r g b a) = Oklcha' (Oklch' lch) a
  where
    lch = rgb2oklch__ [r,g,b]

toColourMaybe :: Oklcha -> Maybe Colour
toColourMaybe ok = bool Nothing (Just c) (validColour c)
  where
    c = view oklch' ok

validColour :: Colour -> Bool
validColour (Colour r g b o) = r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1 && o >= 0 && o <= 1

-- >>> oklch2rgb  [0.5968470888515913, 0.1562144161795396, 49.76230756891311]
-- [0.7760971665842135, 0.36342661872470994, 2.451958871169441e-2]
oklch2rgb__ :: Array '[3] Double -> Array '[3] Double
oklch2rgb__ = xyz2rgb' . oklab2xyz . oklch2oklab

rgb2oklch__ :: Array '[3] Double -> Array '[3] Double
rgb2oklch__ = oklab2oklch . xyz2oklab . rgb2xyz'

blendWithOk :: Double -> Colour -> Colour -> Colour
blendWithOk x c0 c1 = view oklch' (blendOklcha x (review oklch' c0) (review oklch' c1))

blendWithOkMaybe :: Double -> Colour -> Colour -> Maybe Colour
blendWithOkMaybe x c0 c1 = bool Nothing (Just c) (validColour c)
  where
    c = blendWithOk x c0 c1

blendOklcha :: Double -> Oklcha -> Oklcha -> Oklcha
blendOklcha x (Oklcha l c h a) (Oklcha l' c' h' a') = Oklcha l'' c'' h'' a''
  where
    l'' = l + x * (l' - l)
    c'' = c + x * (c' - c)
    h'' = h + x * (h' - h)
    a'' = a + x * (a' - a)

blendOklch :: Double -> Oklch -> Oklch -> Oklch
blendOklch x (Oklch l c h) (Oklch l' c' h') = Oklch l'' c'' h''
  where
    l'' = l + x * (l' - l)
    c'' = c + x * (c' - c)
    h'' = h + x * (h' - h)

gradientChart :: Int -> Colour -> Colour -> [Chart]
gradientChart grain c0 c1 =
  (\(r,c) -> RectChart (defaultRectStyle & #color .~ c & #borderSize .~ 0) [r]) .
  (\x -> (Rect x (x+d) 0 1, blendWithOk x c0 c1)) <$>
  grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

gradientChartMaybe :: Int -> Colour -> Colour -> [Chart]
gradientChartMaybe grain c0 c1 = mconcat $
  (\(r,c) -> [RectChart (defaultRectStyle & #color .~ fromMaybe transparent c & #borderSize .~ 0) [r]]) .
  (\x -> (Rect x (x+d) 0 1, blendWithOkMaybe x c0 c1)) <$>
  grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

gradientChartOk :: Int -> Oklcha -> Oklcha -> [Chart]
gradientChartOk grain c0 c1 =
  (\(r,c) -> RectChart (defaultRectStyle & #color .~ c & #borderSize .~ 0) [r]) .
  (\x -> (Rect x (x+d) 0 1, view oklch' (blendOklcha x c0 c1))) <$>
  grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

gradientChartOkMaybe :: Int -> Oklcha -> Oklcha -> [Chart]
gradientChartOkMaybe grain c0 c1 =
  (\(r,c) -> RectChart (defaultRectStyle & #color .~ fromMaybe transparent c & #borderSize .~ 0) [r]) .
  (\x -> (Rect x (x+d) 0 1, toColourMaybe (blendOklcha x c0 c1))) <$>
  grid LowerPos (Range 0 1) grain
  where
    d = 1 / fromIntegral grain

gradient :: Double -> Double -> Int -> Oklcha -> Oklcha -> ChartSvg
gradient h fa grain ok0 ok1 =
  mempty &
  #svgOptions % #svgHeight .~ h &
  #svgOptions % #cssOptions % #shapeRendering .~ UseCssCrisp &
  #hudOptions .~
  ( mempty &
    #chartAspect .~ FixedAspect fa &
    #frames .~ [(20, FrameOptions (Just (border 0.004 white)) 0.1)]) &
  #charts .~ named "gradient" (gradientChartOk grain ok0 ok1)

gradientb :: Double -> Double -> Double -> Int -> Oklcha -> Oklcha -> ChartSvg
gradientb marker h fa grain ok0 ok1 = gradient h fa grain ok0 ok1 & #charts %~ (<> named "border" [borderStrip 0.02 white (Rect (marker - 0.02) (marker + 0.02) (-0.1) 1.1)])

instance (Elevator e, UniformRange e) => Uniform (Color (S.XYZ i) e)
  where
    uniformM g = do
      x <- uniformRM (minValue, maxValue) g
      y <- uniformRM (minValue, maxValue) g
      z <- uniformRM (minValue, maxValue) g
      pure (S.ColorXYZ x y z)

instance Uniform (Color RGB Double)
  where
    uniformM = fmap (S.unColorRGB . (S.xyz2rgb :: Color (S.XYZ S.D65) Double -> Color (S.SRGB 'S.NonLinear) Double)) . uniformM

rXYZs :: [Color (S.XYZ i) Double]
rXYZs = go g0
  where
    g0 = mkStdGen 42
    go g = let (x,g') = uniform g in x:go g'

-- >>> sum $ bool 0 1 . inGamut <$> take 10000 rRGBs
-- 1979
rRGBs_ :: [Color RGB Double]
rRGBs_ = go g0
  where
    g0 = mkStdGen 42
    go g = let (x,g') = uniform g in x:go g'

rRGBClippeds :: [Color RGB Double]
rRGBClippeds = go g0
  where
    g0 = mkStdGen 42
    go g = let (x,g') = uniform g in clip x:go g'

-- >>> sum $ bool 0 1 . inGamut <$> take 10000 rRGBs
-- 1979
rRGBs :: [Color RGB Double]
rRGBs = go g0
  where
    g0 = mkStdGen 42
    go g = let (x,g') = uniform g in
      bool id (x:) (inGamut x) (go g')

inGamut :: (Ord a, Elevator a) => Color RGB a -> Bool
inGamut (ColorRGB r g b) =
  (r >= minValue) && (r <= maxValue) &&
  (g >= minValue) && (g <= maxValue) &&
  (b >= minValue) && (b <= maxValue)

inGamut' :: Array '[3] Double -> Bool
inGamut' a =
  (r >= 0) && (r <= 1) &&
  (g >= 0) && (g <= 1) &&
  (b >= 0) && (b <= 1)
  where
          r = a `index` [0]
          g = a `index` [1]
          b = a `index` [2]

clip :: (Ord a, Elevator a) => Color RGB a -> Color RGB a
clip (ColorRGB r g b) =
  ColorRGB
  (max minValue (min maxValue r))
  (max minValue (min maxValue g))
  (max minValue (min maxValue b))

clip' :: (Ord a) => a -> a -> Array '[3] a -> Array '[3] a
clip' minv maxv = fmap (max minv . min maxv)

-- >>> ty (ColorRGB 0 1 1)
-- <Y * D65:( 0.7874000000000002)>
rgb2luv :: Color RGB Double -> Color (LUV.LUV S.D65) Double
rgb2luv c = convert (S.mkColorRGB c :: Color (S.SRGB 'S.Linear) Double) :: Color (LUV.LUV S.D65) Double

rgbi2luv :: Int -> Int -> Int -> Color (LUV.LUV S.D65) Double
rgbi2luv r g b = convert (S.mkColorRGB (ColorRGB (d r) (d g) (d b)) :: Color (S.SRGB 'S.Linear) Double) :: Color (LUV.LUV S.D65) Double
  where
    d x = fromIntegral x / 256.0

luv2rgb :: Double -> Double -> Double -> Color RGB Double
luv2rgb l u v = S.unColorRGB (convert (LUV.ColorLUV l u v :: Color (LUV.LUV S.D65) Double) :: Color (S.SRGB 'S.Linear) Double)

rgbi2rgb :: Int -> Int -> Int -> Color RGB Double
rgbi2rgb r g b = ColorRGB (d r) (d g) (d b)
  where
    d x = fromIntegral x / 256.0

rgb2rgbt :: Double -> Double -> Double -> Text
rgb2rgbt r g b = "rgb(" <> pack (show (i r)) <> " " <> pack (show (i g)) <> " " <> pack (show (i b)) <> ")"
  where
    i x = floor $ x * 256.0 :: Int

mkSpan :: Color RGB Double -> Text
mkSpan (ColorRGB r g b) =
  "<span style=\"color:rgb(" <> pack (show (i r)) <> "," <> pack (show (i g)) <> "," <> pack (show (i b)) <> ");\">⬤</span>"
  where
    i x = floor (x * 256) :: Int

luv2span :: Double -> Double -> Double -> Text
luv2span l u v = mkSpan $ luv2rgb l u v

-- assumes SRGB NonLinear is the css color space
--
-- >>> hex2xyz "#ffffff"
-- <XYZ * D65:( 0.9505000000000000, 1.0000000000000000, 1.0890000000000000)>
hex2xyz :: Text -> Color (S.XYZ S.D65) Double
hex2xyz t = S.rgb2xyz (S.mkColorRGB $ unsafeFromHex t :: Color (S.SRGB 'S.NonLinear) Double)

data CSSOpaque = CSSTransparent | CSSHex Text | CSSName Text | CSSRGBI Word8 Word8 Word8 | CSSRGB Double Double Double | CSSSRGB Double Double Double | CSSHSL Double Double Double | CSSHWB Double Double Double | CSSLab Double Double Double | CSSLch Double Double Double | CSSOklab Double Double Double | CSSOklch Double Double Double deriving (Eq, Show, Generic)

data CSSColor = CSSColor { cssColor :: CSSOpaque, alpha :: Maybe Double} deriving (Eq, Show, Generic)

{-
cssShow :: CSSColor -> Text
cssShow CSSTransparent = "transparent"
cssShow (CSSColor (CSSHex t) a) = t <>
cssShow (CSSHex t) = t

-}

showSwatch :: Array '[3] Word8 -> Text
showSwatch a =
      [trimming|<div class=swatch style="background:rgb($r $g $b);"></div>|]
        where
          r = pack $ show $ a `index` [0]
          g = pack $ show $ a `index` [1]
          b = pack $ show $ a `index` [2]

showRGB :: Array '[3] Word8 -> Text
showRGB a =
      [trimming|rgb($r $g $b)|]
        where
          r = pack $ show $ a `index` [0]
          g = pack $ show $ a `index` [1]
          b = pack $ show $ a `index` [2]

showRGBs :: Text -> [Array '[3] Word8] -> Text
showRGBs l = (l <>) . Text.intercalate " " . fmap showRGB

showSwatches :: Text -> [Array '[3] Word8] -> Text
showSwatches t hs =
  [trimming|<div>
$divs
$t
</div>
|]
    where
      divs = Text.intercalate "\n" (showSwatchWithRGB <$> hs)


showHexSwatch :: Text -> Text
showHexSwatch h =
      [trimming|<div class=swatch style="background:rgb($r $g $b);"></div>|]
        where
          (ColorRGB rd gd bd) = unsafeFromHex h
          r = pack $ show $ w rd
          g = pack $ show $ w gd
          b = pack $ show $ w bd
          w x = floor $ x * 256.0 :: Int

showHexSwatches :: [Text] -> Text
showHexSwatches hs =
  [trimming|<div>
$divs
</div>
|]
    where
      divs = Text.intercalate "\n" (showHexSwatch <$> hs)

showSwatchWithRGB :: Array '[3] Word8 -> Text
showSwatchWithRGB a =
      [trimming|<div class=swatch style="background:rgb($r $g $b);">($r $g $b)</div>|]
        where
          r = pack $ show $ a `index` [0]
          g = pack $ show $ a `index` [1]
          b = pack $ show $ a `index` [2]

rgb2luvlch :: Color RGB Double -> Color (LUVLCH.LCHuv S.D65) Double
rgb2luvlch c = convert (S.mkColorRGB c :: Color (S.SRGB 'S.Linear) Double) :: Color (LUVLCH.LCHuv S.D65) Double

xy2ch :: (Double, Double) -> (Double, Double)
xy2ch (x,y) = (180 / pi * mod' (angle (Point x y)) (2 * pi), norm (Point x y))

ch2xy :: (Double, Double) -> (Double, Double)
ch2xy (sat,hue) = (x,y)
  where
    (Point x y) = coord (Polar sat (pi / 180 * hue))

mod' :: Double -> Double -> Double
mod' x d = x - fromIntegral (floor (x / d)) * d

m1 :: Array '[3,3] Double
m1 =
  [ 0.8189330101,0.3618667424,-0.1288597137,
    0.0329845436,0.9293118715,0.0361456387,
    0.0482003018,0.2643662691,0.6338517070
  ]

m2 :: Array '[3,3] Double
m2 =
  [ 0.2104542553,0.7936177850,-0.0040720468,
    1.9779984951,-2.4285922050,0.4505937099,
    0.0259040371,0.7827717662,-0.8086757660
  ]

cubicroot :: (Floating a, Ord a) => a -> a
cubicroot x = bool (-1*(-x)**(1/3.0)) (x**(1/3.0)) (x>=0)

-- >>> xyz2oklab [0.95, 1, 1.089]
-- [0.9999686754143632, -2.580058168537569e-4, -1.1499756458199784e-4]
--
-- >>> xyz2oklab [1,0,0]
-- [0.4499315814860224, 1.2357102101076207, -1.9027581087245393e-2]
--
-- >>> xyz2oklab [0,1,0]
-- [0.921816758286376, -0.6712376131199635, 0.2633235500611929]
--
-- >>> xyz2oklab [1,0,1]
-- [0.5081033967278659, 1.147837087146462, -0.36768466477695416]
--
-- >>> xyz2oklab [0,0,1]
-- [0.15260258004008057, -1.4149965510120839, -0.4489272035597538]
xyz2oklab :: Array '[3] Double -> Array '[3] Double
xyz2oklab xyz =
  dot sum (*) m2 (cubicroot <$> dot sum (*) m1 xyz)

m1' :: Array '[3,3] Double
m1' = [ 1.227013851103521026, -0.5577999806518222383, 0.28125614896646780758,
        -0.040580178423280593977, 1.1122568696168301049, -0.071676678665601200577,
        -0.076381284505706892869, -0.42148197841801273055, 1.5861632204407947575
      ]

m2' :: Array '[3,3] Double
m2' = [ 0.99999999845051981432, 0.39633779217376785678, 0.21580375806075880339,
        1.0000000088817607767, -0.1055613423236563494, -0.063854174771705903402,
        1.0000000546724109177, -0.089484182094965759684, -1.2914855378640917399
      ]


-- convert from oklab to xyz
-- >>> oklab2xyz [1,0,0]
-- [0.9504700449825851, 1.0000000305967707, 1.088300206799738]
--
-- >>> oklab2xyz [0.45,1.236,-0.019]
-- [1.0006043815535572, -7.984686748605408e-6, -3.832477084107777e-5]
--
-- >>> oklab2xyz [0.922,-0.671,0.263]
-- [3.048545364080918e-4, 1.0005040791459316, 8.980327498334106e-4]
--
-- >>> oklab2xyz [0.153,-1.415,-0.449]
-- [5.895440766338078e-4, 5.710479039887362e-5, 1.0016497078983162]
oklab2xyz :: Array '[3] Double -> Array '[3] Double
oklab2xyz lab =
  dot sum (*) m1' ((**3.0) <$> dot sum (*) m2' lab)


-- >>> rgb2linear 0.5
-- 0.7353569830524495
rgb2linear :: Double -> Double
rgb2linear x = bool (12.92*x) (1.055 * (x**(1.0/2.4)) - 0.055) (x >= 0.0031308)

-- >>> linear2rgb 0.735
-- 0.4994581635400763
linear2rgb :: Double -> Double
linear2rgb x = bool (x/12.92) (((x + 0.055)/(1 + 0.055)) ** 2.4) (x >= 0.04045)

-- >>> rgb2xyz' 255 255 255
-- <XYZ * D65:( 0.9505000000000000, 1.0000000000000000, 1.0890000000000000)>
rgbw2xyz' :: Array '[3] Word8 -> Color (S.XYZ S.D65) Double
rgbw2xyz' x = S.rgb2xyz (S.mkColorRGB (toDouble <$> (ColorRGB (x `index` [0]) (x `index` [1]) (x `index` [2]) :: Color RGB Word8)) :: Color (S.SRGB 'S.NonLinear) Double) :: Color (S.XYZ S.D65) Double

-- >>> rgb2xyz' 255 255 255
-- <XYZ * D65:( 0.9505000000000000, 1.0000000000000000, 1.0890000000000000)>
rgb2xyz' :: Array '[3] Double -> Array '[3] Double
rgb2xyz' a = fromList [x,y,z]
  where
    (S.ColorXYZ x y z) = S.rgb2xyz (S.ColorSRGB (a `index` [0]) (a `index` [1]) (a `index` [2])) :: Color (S.XYZ S.D65) Double


-- example 42 in <https://www.w3.org/TR/css-color-4/#funcdef-lch css-color-4>
--
-- >>> rgb2oklab [0.4906,0.1387,0.159]
-- [0.401001472390226, 0.11472461891177022, 4.530943969552642e-2]
rgb2oklab :: Array '[3] Double -> Array '[3] Double
rgb2oklab = xyz2oklab . rgb2xyz'

oklab2oklch :: Array '[3] Double -> Array '[3] Double
oklab2oklch a = fromList [a `index` [0], sat, hue ]
  where
    (hue, sat) = xy2ch (a `index` [1], a `index` [2])

-- >>> rgb2oklch [0.4906,0.1387,0.159]
-- [0.401001472390226, 0.1233478151811918, 21.551088737136467]
--
-- >>> rgb2oklch [0.7761, 0.3634, 0.0245]
-- [0.5968470888515913, 0.1562144161795396, 49.76230756891311]
-- >>>  rgb2oklch ([0.3829,0.6727, 0.9385])
-- [0.723228969579331, 0.1240095137439407, 248.0021332482873]
rgb2oklch :: Array '[3] Double -> Array '[3] Double
rgb2oklch = oklab2oklch . xyz2oklab . rgb2xyz'

-- >>> oklch2rgb  [0.5968470888515913, 0.1562144161795396, 49.76230756891311]
-- [0.7760971665842135, 0.36342661872470994, 2.451958871169441e-2]
oklch2rgb :: Array '[3] Double -> Array '[3] Double
oklch2rgb = xyz2rgb' . oklab2xyz . oklch2oklab

-- | convert oklch to rgb, clipping ou-of-gamut results
oklch2rgb' :: Array '[3] Double -> Array '[3] Double
oklch2rgb' = clip' 0 0.9999 . xyz2rgb' . oklab2xyz . oklch2oklab

-- | convert oklch to maybe a rgb, if in gamut
oklch2rgb_ :: Array '[3] Double -> Maybe (Array '[3] Double)
oklch2rgb_ = (\x -> bool Nothing (Just x) (inGamut' x)) . xyz2rgb' . oklab2xyz . oklch2oklab

oklch2oklab :: Array '[3] Double -> Array '[3] Double
oklch2oklab a = fromList [a `index` [0], x, y]
  where
    (x, y) = ch2xy (a `index` [1], a `index` [2])

xyz2rgb' :: Array '[3] Double -> Array '[3] Double
xyz2rgb' a = fromList [r,g,b]
  where
    (S.ColorSRGB r g b) = S.xyz2rgb (S.ColorXYZ (a `index` [0]) (a `index` [1]) (a `index` [2])) :: Color (S.SRGB 'S.NonLinear) Double

rgbw2oklch :: Array '[3] Word8 -> Array '[3] Double
rgbw2oklch w = rgb2oklch ((\x -> fromIntegral x/256.0) <$> w)

oklch2greyscale :: Array '[3] Double -> Array '[3] Double
oklch2greyscale a = oklch2rgb $ fromList [a `index` [0], 0, 0]

oklch2l :: Array '[3] Double -> Array '[3] Double
oklch2l a = fromList [a `index` [0], 0, a `index` [2]]

rgb2rgbw :: Array '[3] Double -> Array '[3] Word8
rgb2rgbw = fmap (floor . (256*))

greys :: Array '[3] Word8 -> Array '[3] Word8
greys = fmap (floor . (256*)) . oklch2greyscale . rgbw2oklch

chroma :: Double -> Array '[3] Word8 -> Array '[3] Word8
chroma l = fmap (floor . (256*)) . oklch2rgb' . (\a -> fromList [l, a `index` [1], a `index` [2]]) . rgbw2oklch

hues :: Double -> Double -> Array '[3] Word8 -> Array '[3] Word8
hues l sat = fmap (floor . (256*)) . oklch2rgb' . (\a -> fromList [l, sat, a `index` [2]]) . rgbw2oklch

sats :: Double -> Double -> Array '[3] Word8 -> Array '[3] Word8
sats l hue = fmap (floor . (256*)) . oklch2rgb' . (\a -> fromList [l, a `index` [1], hue]) . rgbw2oklch

sats_ :: Double -> Double -> Array '[3] Word8 -> Maybe (Array '[3] Word8)
sats_ l hue = fmap (fmap (floor . (256*))) . oklch2rgb_ . (\a -> fromList [l, a `index` [1], hue]) . rgbw2oklch

oklch2hue :: Double -> Double -> Array '[3] Double -> Array '[3] Double
oklch2hue l sat a = oklch2rgb $ fromList [l, sat, a `index` [2]]

wheel :: Int -> Double -> Double -> [(Point Double, Colour)]
wheel grain l maxsat = (\(Point sat hue) -> (uncurry Point $ ch2xy (sat,hue), (\a -> Colour (a `index` [0]) (a `index` [1]) (a `index` [2]) 1) $ oklch2rgb (fromList [l,sat,hue]))) <$> grid LowerPos (Rect 0 maxsat 0 360) (Point grain grain)

borderStrip :: Double -> Colour -> Rect Double -> Chart
borderStrip w c r = RectChart (defaultRectStyle & #color .~ transparent & #borderSize .~ w & #borderColor .~ c) [r]

oklch2colour :: Array '[3] Double -> Double -> Colour
oklch2colour c = Colour (c' `index` [0]) (c' `index` [1]) (c' `index` [2])
  where
    c' = oklch2rgb c

oklch2colour_ :: Array '[3] Double -> Double -> Maybe Colour
oklch2colour_ c a = (\x -> Colour (x `index` [0]) (x `index` [1]) (x `index` [2]) a) <$> c'
  where
    c' = oklch2rgb_ c


rgb2Colour :: Array '[3] Double -> Colour
rgb2Colour a = Colour (a `index` [0]) (a `index` [1]) (a `index` [2]) 1

rgbw2rgb :: Array '[3] Word8 -> Array '[3] Double
rgbw2rgb = fmap (\x -> fromIntegral x/256.0)

oklch2point :: Array '[3] Double -> Point Double
oklch2point a = uncurry Point $ ch2xy (a `index` [1],a `index` [2])

rgb2pc :: Array '[3] Word8 -> (Point Double, Colour)
rgb2pc a = (oklch2point . rgb2oklch . rgbw2rgb $ a, rgb2Colour . rgbw2rgb $ a)

dot' :: Array '[3] Word8 -> Chart
dot' x = (\(p,c) -> GlyphChart (defaultGlyphStyle & #size .~ 0.08 & #color .~ c & #borderColor .~ Colour 0.5 0.5 0.5 1 & #shape .~ CircleGlyph) [p]) (rgb2pc x)

rgb2colour :: Array '[3] Double -> Colour
rgb2colour a = Colour (a `index` [0]) (a `index` [1]) (a `index` [2]) 1

rgbw2colour :: Array '[3] Word8 -> Colour
rgbw2colour = rgb2colour . rgbw2rgb

