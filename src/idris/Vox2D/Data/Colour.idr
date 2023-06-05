module Vox2D.Data.Colour

||| RGB colour with channel width of 1 byte
public export
record Colour where
  constructor MkColour
  r : Bits8
  g : Bits8
  b : Bits8

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
