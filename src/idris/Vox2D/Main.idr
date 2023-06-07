module Vox2D.Main

import Data.Maybe

import Vox2D.Shape.Flat.Line
import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector

import Vox2D.Shape.Solid.Circle
import Vox2D.Shape.Solid.Closure
import Vox2D.Shape.Solid.Rectangle

import Vox2D.Data.Colour
import Vox2D.Data.PPM
import Vox2D.Data.StrBuffer
import Vox2D.Data.Material

import Vox2D.Sampling

center : Point
center = MkPoint 0 0

right : Vector
right = MkVector 1 0

up : Vector
up = MkVector 0 1

myRec : Rectangle
myRec = MkRectangle center right up

myLeftCircle : Circle
myLeftCircle = MkCircle (MkPoint (-1) 0) 1

myRightCircle : Circle
myRightCircle = MkCircle (MkPoint 1 0) 1

myBasis : Basis
myBasis = MkBasis center (scale right 3) (scale up 3)

myResolution : Resolution
myResolution = MkResolution 1920 1080

myLights : List PointLight
myLights = [MkPointLight (MkPoint 0 4) (MkLight 10 10 10)]

{- main : IO ()
main = do
  buf <- readFromFile "assets/dirt.ppm"
  ppm <- fromStrBuffer buf
  buf <- toStrBuffer ppm
  writeToFile buf "test-dirt.ppm" -}

||| Maps arbitrary point in the rectangle to (u, v) coordinates of AABB in the atlas
mapRectangleToAABB : Rectangle
                  -> (aabbU : Nat)
                  -> (aabbV : Nat)
                  -> (aabbExtW : Nat)
                  -> (aabbExtH : Nat)
                  -> Point
                  -> (Nat, Nat)
mapRectangleToAABB (MkRectangle c r u) aabbU aabbV aabbExtW aabbExtH p =
  let d = displacement c p in
  let x = dot d r / length r in
  let y = dot d u / length u in
  let ucoord = cast aabbU + x * cast aabbExtW in
  let vcoord = cast aabbV - y * cast aabbExtH in
  (cast ucoord, cast vcoord)

||| Maps arbitrary point in the circle to (u, v) coordinates of circle in the atlas
mapCircleToCircle : Circle
                 -> (cu : Nat)
                 -> (cv : Nat)
                 -> (r' : Nat)
                 -> Point
                 -> (Nat, Nat)
mapCircleToCircle (MkCircle c r) cu cv r' p =
  let d = displacement c p in
  let cosAlpha = d.x / length d in
  let cosBeta = d.y / length d in
  let u = cast cu + length d * (cast r' / r) * cosAlpha in
  let v = cast cv + length d * (cast r' / r) * cosBeta in
  (cast u, cast v)

public export
toMaterial : Colour -> Material
toMaterial (MkColour r g b) = MkMaterial (cast r / 255) (cast g / 255) (cast b / 255)

main : IO ()
main = do
  atlas <- readFromFile "assets/dirt.ppm" >>= fromStrBuffer
  let myShape =
    Union (Union (InjRectangle myRec (textureMapMyRectangle atlas))
                 (InjCircle myLeftCircle (textureMapMyLeftCircle atlas)))
                 (InjCircle myRightCircle (textureMapMyRightCircle atlas))
  let img = raytrace myBasis myShape myLights atlas black myResolution
  let ppm = toPPM img
  writeToFile !(toStrBuffer ppm) "myShape.ppm"
 where
   textureMapMyRectangle : PPM -> Point -> Material
   textureMapMyRectangle atlas p =
     let (u, v) = mapRectangleToAABB myRec 40 40 22 22 p in
     let (r, g, b) = fromMaybe (0, 0, 0) (at atlas u v) in
     let c = MkColour (cast r) (cast g) (cast b) in
     toMaterial c

   textureMapMyLeftCircle : PPM -> Point -> Material
   textureMapMyLeftCircle atlas p =
     let (u, v) = mapCircleToCircle myLeftCircle 112 72 36 p in
     let (r, g, b) = fromMaybe (0, 0, 0) (at atlas u v) in
     let c = MkColour (cast r) (cast g) (cast b) in
     toMaterial c

   textureMapMyRightCircle : PPM -> Point -> Material
   textureMapMyRightCircle atlas p =
     let (u, v) = mapCircleToCircle myRightCircle 112 72 36 p in
     let (r, g, b) = fromMaybe (0, 0, 0) (at atlas u v) in
     let c = MkColour (cast r) (cast g) (cast b) in
     toMaterial c
