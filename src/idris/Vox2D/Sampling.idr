module Vox2D.Sampling

import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Line

import Vox2D.Shape.Solid.Circle
import Vox2D.Shape.Solid.Rectangle
import Vox2D.Shape.Solid.Closure

import Vox2D.Data.Colour
import Vox2D.Data.PPM
import Vox2D.Data.Material

import Data.Buffer

public export
record Resolution where
  constructor MkResolution
  width : Bits32
  height : Bits32

||| Rectangular grid of RGB colours with channel width of 1 byte.
public export
record Img where
  constructor MkImg
  width : Bits32
  height : Bits32
  ||| RGB 3-byte-per-pixel img
  pixels : List Colour
  -- Invariant: length pixels = width * height

||| Orthogonal basis in ℝ²
public export
record Basis where
  constructor MkBasis
  center : Point
  right : Vector
  up : Vector
  -- invariant: right ⊥ up

||| RGB-encoded light.
public export
record Light where
  constructor MkLight
  r : Double
  g : Double
  b : Double

public export
darkness : Light
darkness = MkLight 0 0 0

public export
reflect : Light -> Material -> Light
reflect (MkLight r g b) (MkMaterial kr kg kb) =
  MkLight (r * kr) (g * kg) (b * kb)

public export
(+) : Light -> Light -> Light
MkLight r0 g0 b0 + MkLight r1 g1 b1 = MkLight (r0 + r1) (g0 + g1) (b0 + b1)

public export
scale : Light -> Double -> Light
scale (MkLight r g b) k = MkLight (r * k) (g * k) (b * k)

public export
toColour : Light -> Colour
toColour (MkLight r g b) = MkColour (cast $ min 255 (r * 255))
                                    (cast $ min 255 (g * 255))
                                    (cast $ min 255 (b * 255))

||| Point light w.r.t. an orthogonal basis in ℝ².
public export
record PointLight where
  constructor MkPointLight
  pt : Point
  intensity : Light

public export
raytrace : Basis
        -> Closure
        -> List PointLight
        -> (textureAtlas : PPM) -- TODO: switch to an IR
        -> (missColour : Colour)
        -> Resolution
        -> Img --TODO: Wrap in a monad to support sensible error-handing
raytrace basis closure lights atlas missColour res =
 let stepU = 2.0 * length basis.right / cast res.width in
 let stepV = 2.0 * length basis.up / cast res.height in
 let o = offset (offset basis.center (neg basis.right)) basis.up in
 let pixels = sample o stepU stepV in
 MkImg res.width res.height pixels
 where
  samplePoint : (topLeftCorner : Point)
             -> (stepU : Double)
             -> (stepV : Double)
             -> (row : Bits32)
             -> (col : Bits32)
             -> Point
  samplePoint o stepU stepV row col =
    offset o (MkVector (stepU * 0.5 + stepU * cast col) (-(stepV * 0.5 + stepV * cast row)))


  applyLight : PointLight -> Point -> Material -> Light
  applyLight light pt material =
    scale (reflect light.intensity material) (1 / length2 (displacement pt light.pt))

  applyLights : List PointLight -> Point -> Material -> Light
  applyLights lights pt mat =
    sum $ map (\l => applyLight l pt mat) lights
   where
    sum : List Light -> Light
    sum [] = darkness
    sum (x :: xs) = x + sum xs

  samplePixel : (topLeftCorner : Point)
             -> (stepU : Double)
             -> (stepV : Double)
             -> (row : Bits32)
             -> (col : Bits32)
             -> Colour
  samplePixel o stepU stepV row col =
    let pt = samplePoint o stepU stepV row col in
    case belongs pt closure of
      True =>
        let hitMaterial = textureMap closure pt in
        toColour $ applyLights lights pt hitMaterial
      False => missColour

  sampleRowH : (topLeftCorner : Point)
            -> (stepU : Double)
            -> (stepV : Double)
            -> (row : Bits32)
            -- invariant: curCol ≤ numCol
            -> (curCol : Bits32)
            -> (numCol : Bits32)
            -> List Colour
  sampleRowH o stepU stepV row curCol numCol =
    case curCol == numCol of
      True => []
      False =>
       samplePixel o stepU stepV row curCol ::
         sampleRowH o stepU stepV row(curCol + 1) numCol

  sampleRow : (topLeftCorner : Point)
           -> (stepU : Double)
           -> (stepV : Double)
           -> (row : Bits32)
           -> List Colour
  sampleRow o stepU stepV row =
    sampleRowH o stepU stepV row 0 res.width

  sampleH : (topLeftCorner : Point)
         -> (stepU : Double)
         -> (stepV : Double)
            -- invariant: curRow ≤ numRows
         -> (curRow : Bits32)
         -> (numRows : Bits32)
         -> List Colour
  sampleH o stepU stepV curRow numRows =
    case curRow == numRows of
      True => []
      False =>
        sampleRow o stepU stepV curRow
          ++
        sampleH o stepU stepV (curRow + 1) numRows

  sample : (topLeftCorner : Point)
        -> (stepU : Double)
        -> (stepV : Double)
        -> List Colour
  sample o stepU stepV = sampleH o stepU stepV 0 res.height

public export
toPPM : Img -> PPM
toPPM (MkImg w h pixels) = MkPPM w h 255 (encodeColours pixels)
 where
  encodeColour : Colour -> List Bits16
  encodeColour (MkColour r g b) = [cast r, cast g, cast b]

  encodeColours : List Colour -> List Bits16
  encodeColours list = do
   c <- list
   encodeColour c
