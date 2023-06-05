module Vox2D.Shape.Solid.Rectangle

import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Line

||| Given a basis in ℝ², Rectangle represents a solid closed rectangle in ℝ² relative to the basis.
public export
record Rectangle where
  constructor MkRectangle
  center : Point
  extW : Vector
  extH : Vector

namespace Rectangle
  public export
  belongs : Point -> Rectangle -> Bool
  belongs pt (MkRectangle c w h) =
    let p0 = offset c (neg w)
        line0 = MkLine p0 h
        p1 = offset c h
        line1 = MkLine p1 w
        p2 = offset c w
        line2 = MkLine p2 (neg h)
        p3 = offset c (neg h)
        line3 = MkLine p3 (neg w)
    in
    weaklyOnTheRight pt line0
      &&
    weaklyOnTheRight pt line1
      &&
    weaklyOnTheRight pt line2
      &&
    weaklyOnTheRight pt line3

