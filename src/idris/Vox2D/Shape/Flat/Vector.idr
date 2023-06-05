module Vox2D.Shape.Flat.Vector

||| Given a basis in ℝ²
||| Vector represents a geometric vector in ℝ² relative to the basis.
public export
record Vector where
  constructor MkVector
  dx : Double
  dy : Double

public export
neg : Vector -> Vector
neg (MkVector dx dy) = MkVector (-dx) (-dy)

public export
scale : Vector -> Double -> Vector
scale (MkVector dx dy) k = MkVector (dx * k) (dy * k)

||| Dot-product (scalar-product) of two vectors.
public export
dot : Vector -> Vector -> Double
dot (MkVector dx0 dy0) (MkVector dx1 dy1) = dx0 * dx1 + dy0 * dy1

public export
length2 : Vector -> Double
length2 v = dot v v

public export
length : Vector -> Double
length v = sqrt (dot v v)

||| Given a vector, returns a vector of the same length, orthogonal
||| to the first, such that the new vector looks to the right w.r.t. the first one.
|||
|||      w
||| - - - - - >
||| |
||| |
||| | ortho(w)
||| |
||| v
||| |w| = |ortho(w)|
public export
ortho : Vector -> Vector
ortho (MkVector dx dy) = MkVector dy (-dx)

