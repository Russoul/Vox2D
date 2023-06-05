module Vox2D.Shape.Flat.Line

import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Point

||| Given a basis in ℝ²
||| Line represents an infinite line in ℝ² relative to the basis.
public export
record Line where
  constructor MkLine
  pt : Point
  dir : Vector

||| Given a line, returns a vector orthogonal to its direction.
||| Length of that vector is equal to that of the direction.
||| Normal vector looks to the right w.r.t. the direction vector.
public export
normal : Line -> Vector
normal (MkLine pt dir) = ortho dir

||| Check if the point is on the right of the line
||| or on the line itself
public export
weaklyOnTheRight : Point -> Line -> Bool
weaklyOnTheRight pt line =
  (normal line `dot` displacement line.pt pt) >= 0
