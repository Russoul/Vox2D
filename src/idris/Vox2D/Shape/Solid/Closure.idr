module Vox2D.Shape.Solid.Closure

import Vox2D.Shape.Flat.Point
import Vox2D.Shape.Flat.Vector
import Vox2D.Shape.Flat.Line

import Vox2D.Shape.Solid.Circle
import Vox2D.Shape.Solid.Rectangle

||| Given a basis in ℝ², the shape language
||| Closure represents a subset of closed solid shapes in ℝ² relative to the basis.
public export
data Closure : Type where
  ||| Solid circle shifted (`dx`, `dy`) from the basis origin along the basis axes
  ||| of radius `rad`. (dx = 0, dy = 0) corresponds to a circle whose center coincides with the basis origin.
  InjCircle : Circle -> Closure
  ||| Solid square shifted (`dx`, `dy`) from the basis origin along the basis axes
  ||| with side of length `side`. All sides are aligned with one of the axes.
  ||| (dx = 0, dy = 0) corresponds to a square whose center coincides with the basis origin.
  InjRectangle : Rectangle -> Closure
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
  belongs pt (InjCircle circle) = belongs pt circle
  belongs pt (InjRectangle rectangle) = belongs pt rectangle
  belongs pt (Union a b) = belongs pt a || belongs pt b
  belongs pt (Intersection a b) = belongs pt a && belongs pt b
  belongs pt (Difference a b) = belongs pt a && not (belongs pt b)

  public export
  intersects : Line -> Closure -> Bool
  intersects l (InjCircle c) = intersects l c
  intersects l (InjRectangle r) = intersects l r
  intersects l (Union a b) = intersects l a || intersects l b
  intersects l (Intersection a b) = intersects l a && intersects l b
  intersects l (Difference a b) = intersects l a && not (intersects l b)
