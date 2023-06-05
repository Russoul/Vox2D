module Vox2D.Shape.Solid.Circle

import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector

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

