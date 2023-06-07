module Vox2D.Shape.Solid.Closure

import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Line

import Vox2D.Shape.Solid.Circle
import Vox2D.Shape.Solid.Rectangle

import Vox2D.Data.Material

public export
TextureMap : Type
TextureMap = Point -> Material

||| Given a basis in ℝ², the shape language
||| Closure represents a subset of closed solid shapes in ℝ² relative to the basis.
||| Every base shape comes with a texture map: every point in that shape must map to (u, v) coordinates on the texture atlas.
|||
||| - - - - - >
||| |
||| |  *(u, v)
||| v
public export
data Closure : Type where
  ||| Solid circle shifted (`dx`, `dy`) from the basis origin along the basis axes
  ||| of radius `rad`. (dx = 0, dy = 0) corresponds to a circle whose center coincides with the basis origin.
  InjCircle : Circle -> TextureMap -> Closure
  ||| Solid square shifted (`dx`, `dy`) from the basis origin along the basis axes
  ||| with side of length `side`. All sides are aligned with one of the axes.
  ||| (dx = 0, dy = 0) corresponds to a square whose center coincides with the basis origin.
  InjRectangle : Rectangle -> TextureMap -> Closure
  ||| Given two shapes we can always form a union of the two.
  ||| A ∪ B
  Union : Closure -> Closure -> Closure
  ||| Given two shapes we can always form an intersection of the two.
  ||| A ∩ B
  Intersection : Closure -> Closure -> Closure
  ||| Given two shapes we can always form a difference of the two.
  ||| A \ B
  Difference : Closure -> Closure -> Closure

namespace Closure
  ||| Check if the given point belongs to the closure-shape.
  public export
  belongs : Point -> Closure -> Bool
  belongs pt (InjCircle circle _) = belongs pt circle
  belongs pt (InjRectangle rectangle _) = belongs pt rectangle
  belongs pt (Union a b) = belongs pt a || belongs pt b
  belongs pt (Intersection a b) = belongs pt a && belongs pt b
  belongs pt (Difference a b) = belongs pt a && not (belongs pt b)

  public export
  intersects : Line -> Closure -> Bool
  intersects l (InjCircle c _) = intersects l c
  intersects l (InjRectangle r _) = intersects l r
  intersects l (Union a b) = intersects l a || intersects l b
  intersects l (Intersection a b) = intersects l a && intersects l b
  intersects l (Difference a b) = intersects l a && not (intersects l b)

  public export
  textureMap : Closure -> Point -> Material
  textureMap (InjCircle _ f) x = f x
  textureMap (InjRectangle _ f) x = f x
  textureMap (Union a b) x =
    let inA = belongs x a in
    let inB = belongs x b in
    case (inA, inB) of
      (True, True) => average (textureMap a x) (textureMap b x)
      (True, False) => textureMap a x
      (False, True) => textureMap b x
      -- Impossible by the assumption that the point must belong to the shape
      (False, False) => blackHole
  textureMap (Intersection a b) x = average (textureMap a x) (textureMap b x)
  textureMap (Difference a b) x = textureMap a x
