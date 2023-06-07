module Vox2D.Algebra.Matrix

import Data.Vect

public export
det2 : Vect 4 Double -> Double
det2 [a, b,
      c, d] = a * d - c * b

||| Assumes that the matrix has an inverse.
public export
inv2 : Vect 4 Double -> Maybe (Vect 4 Double)
inv2 [a, b,
      c, d] =
  let d =
         det2 [a, b,
               c, d]
  in
  case d == 0 of
    True => Nothing
    False => Just $
       map (* 1.0 / d)
            [d, -b,
            -c,  a]

namespace A
  ||| 2⨯2 matrix multiplication
  ||| A * B
  public export
  (*) : Vect 4 Double -> Vect 4 Double -> Vect 4 Double
  [a0, b0, c0, d0] * [a1, b1, c1, d1] =
    [a0 * a1 + b0 * c1, a0 * b1 + b0 * b1, c0 * a1 + d0 * c1, c0 * b1 + d0 * d1]

namespace B
  ||| 2⨯2 matrix multiplied by a vector
  ||| A * v̄
  public export
  (*) : Vect 4 Double -> Vect 2 Double -> Vect 2 Double
  [a, b, c, d] * [x, y] = [a * x + b * y, c * x + d * y]
