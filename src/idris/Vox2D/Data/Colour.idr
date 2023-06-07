module Vox2D.Data.Colour

||| RGB colour with channel width of 1 byte
public export
record Colour where
  constructor MkColour
  r : Bits8
  g : Bits8
  b : Bits8

public export
(*) : Colour -> Colour -> Colour
MkColour r0 g0 b0 * MkColour r1 g1 b1 =
  MkColour (cast {to = Bits8} $ cast {to = Double} r0 * cast {to = Double} r1 / 255)
           (cast {to = Bits8} $ cast {to = Double} g0 * cast {to = Double} g1 / 255)
           (cast {to = Bits8} $ cast {to = Double} b0 * cast {to = Double} b1 / 255)

||| The result is clamped to be no higher than 255 at each channel
public export
scale : Colour -> Double -> Colour
scale (MkColour r g b) k =
  MkColour (min (cast {to = Bits8} $ cast {to = Double} r * k) 255)
           (min (cast {to = Bits8} $ cast {to = Double} g * k) 255)
           (min (cast {to = Bits8} $ cast {to = Double} b * k) 255)

public export
(+) : Colour -> Colour -> Colour
MkColour r0 g0 b0 + MkColour r1 g1 b1 =
  MkColour (cast $ cast {to = Bits64} r0 + cast {to = Bits64} r1)
           (cast $ cast {to = Bits64} g0 + cast {to = Bits64} g1)
           (cast $ cast {to = Bits64} b0 + cast {to = Bits64} b1)

public export
black : Colour
black = MkColour 0 0 0

public export
red : Colour
red = MkColour 255 0 0

public export
green : Colour
green = MkColour 0 255 0

public export
blue : Colour
blue = MkColour 0 0 255
