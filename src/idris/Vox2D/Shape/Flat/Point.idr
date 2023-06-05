module Vox2D.Shape.Flat.Point

import Vox2D.Shape.Flat.Vector

||| Given a basis in ℝ²
||| Point represents a point in ℝ² relative to the basis.
public export
record Point where
  constructor MkPoint
  x : Double
  y : Double

||| Given points A and B, displacement is a vector pointing
||| from A to B.
public export
displacement : Point -> Point -> Vector
displacement (MkPoint x0 y0) (MkPoint x1 y1) = MkVector (x1 - x0) (y1 - y0)

||| Offset a point by a vector.
public export
offset : Point -> Vector -> Point
offset (MkPoint x y) (MkVector dx dy) = MkPoint (x + dx) (y + dy)
