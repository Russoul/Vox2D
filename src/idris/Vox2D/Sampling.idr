module Vox2D.Sampling

import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Line

import Vox2D.Shape.Solid.Circle
import Vox2D.Shape.Solid.Rectangle
import Vox2D.Shape.Solid.Closure

import Vox2D.Data.Colour
import Vox2D.Data.PPM

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

public export
raytrace : Basis -> Closure -> (hitColour : Colour) -> (missColour : Colour) -> Resolution -> Img
raytrace basis closure hitColour missColour res =
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

  samplePixel : (topLeftCorner : Point)
             -> (stepU : Double)
             -> (stepV : Double)
             -> (row : Bits32)
             -> (col : Bits32)
             -> Colour
  samplePixel o stepU stepV row col =
    case belongs (samplePoint o stepU stepV row col) closure of
      True => hitColour
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
