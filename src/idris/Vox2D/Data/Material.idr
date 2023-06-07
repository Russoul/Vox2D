module Vox2D.Data.Material

public export
record Material where
  constructor MkMaterial
  reflectR : Double -- [0;1]
  reflectG : Double -- [0;1]
  reflectB : Double -- [0;1]

public export
blackHole : Material
blackHole = MkMaterial 0 0 0

public export
average : Material -> Material -> Material
average (MkMaterial r0 g0 b0) (MkMaterial r1 g1 b1) =
  MkMaterial ((r0 + r1) / 2) ((g0 + g1) / 2) ((b0 + b1) / 2)
