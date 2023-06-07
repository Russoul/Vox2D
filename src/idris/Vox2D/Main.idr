module Vox2D.Main

import Vox2D.Shape.Flat.Line
import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector

import Vox2D.Shape.Solid.Circle
import Vox2D.Shape.Solid.Closure
import Vox2D.Shape.Solid.Rectangle

import Vox2D.Data.Colour
import Vox2D.Data.PPM
import Vox2D.Data.StrBuffer

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

myShape : Closure
myShape =
  Union (Union (InjRectangle myRec) (InjCircle myLeftCircle)) (InjCircle myRightCircle)

myBasis : Basis
myBasis = MkBasis center (scale right 3) (scale up 3)

myResolution : Resolution
myResolution = MkResolution 1920 1080

myLights : List PointLight
myLights = [MkPointLight (MkPoint 0 4) (MkLight 10 10 10)]

main : IO ()
main = do
  buf <- readFromFile "assets/dirt.ppm"
  ppm <- fromStrBuffer buf
  buf <- toStrBuffer ppm
  writeToFile buf "test-dirt.ppm"

{- main : IO ()
main = do
  let img = raytrace myBasis myShape myLights (MkMaterial 0 1 0) black myResolution
  let ppm = toPPM img
  writeToFile !(toStrBuffer ppm) "myShape.ppm" -}
