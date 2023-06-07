module Vox2D.Shape.Solid.Circle

import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Line

||| Given a basis in ℝ²,
||| Circle represents a solid closed circle in ℝ² relative to the basis.
public export
record Circle where
  constructor MkCircle
  center : Point
  rad : Double

namespace Circle
  public export
  belongs : Point -> Circle -> Bool
  belongs pt (MkCircle center r) =
    length2 (displacement center pt) <= r * r

  public export
  intersects : Line -> Circle -> Bool
  intersects (MkLine o d) (MkCircle c r) =
    4 * (displacement c o `dot` d) * (displacement c o `dot` d) -
      4 * (d `dot` d) * (length2 (displacement c o) - r * r) >= 0

