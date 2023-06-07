module Vox2D.Shape.Flat.Line

import Data.Vect
import Data.Maybe

import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Point

import Vox2D.Algebra.Matrix

||| Given a basis in ℝ²
||| Line represents an infinite line in ℝ² relative to the basis.
public export
record Line where
  constructor MkLine
  pt : Point
  dir : Vector

||| Given a basis in ℝ²
||| Line represents an infinite line in ℝ² relative to the basis.
public export
record LineSegment where
  constructor MkLineSegment
  pt0 : Point
  pt1 : Point

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

public export
toLine : LineSegment -> Line
toLine (MkLineSegment pt0 pt1) = MkLine pt0 (displacement pt0 pt1)

namespace Line
  public export
  intersects : Line -> Line -> Bool
  intersects (MkLine c0 d0) (MkLine c1 d1) =
    det2 [d0.x, -d1.x,
          d0.y, -d1.y] /= 0

namespace Line
  public export
  intersectsAt : Line -> Line -> Maybe (Double, Double)
  intersectsAt l0@(MkLine c0 d0) l1@(MkLine c1 d1) =
    case intersects l0 l1 of
      False => Nothing
      True => do
        i <- inv2 [d0.x, -d1.x,
                   d0.y, -d1.y]
        let [t0, t1] = i * toVect (displacement c0 c1)
        Just (t0, t1)



namespace LineSegment
  public export
  intersectsAt : Line -> LineSegment -> Maybe (Double, Double)
  intersectsAt line@(MkLine c0 d0) seg@(MkLineSegment c1 d1) =
    case intersects line (toLine seg) of
      False => Nothing
      True => do
        (t0, t1) <- intersectsAt line (toLine seg)
        guard (0 <= t1 && t1 <= 1)
        Just (t0, t1)

  public export
  intersects : Line -> LineSegment -> Bool
  intersects l s = isJust (intersectsAt l s)
