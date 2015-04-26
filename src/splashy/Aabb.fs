namespace splashy

open Vector
open Quad

/// axis-aligned bounding boxes: used to draw the bounds of the
/// simulation and also those cells that represent fluid markers.
module Aabb =
  type Aabb = { min_bounds: Vector3d;
                max_bounds: Vector3d }

  let construct a1 a2 =
    { min_bounds = Vector3d.min a1 a2;
      max_bounds = Vector3d.max a1 a2 }

  let contains a (p: Vector3d) =
    p.x >= a.min_bounds.x && p.x <= a.max_bounds.x &&
    p.y >= a.min_bounds.y && p.y <= a.max_bounds.y &&
    p.z >= a.min_bounds.z && p.z <= a.max_bounds.z

  let raw a =
    // eight vertices.
    [| a.min_bounds.x; a.min_bounds.y; a.max_bounds.z; 1.0;
       a.max_bounds.x; a.min_bounds.y; a.max_bounds.z; 1.0;
       a.max_bounds.x; a.max_bounds.y; a.max_bounds.z; 1.0;
       a.min_bounds.x; a.max_bounds.y; a.max_bounds.z; 1.0;
       a.min_bounds.x; a.min_bounds.y; a.min_bounds.z; 1.0;
       a.max_bounds.x; a.min_bounds.y; a.min_bounds.z; 1.0;
       a.max_bounds.x; a.max_bounds.y; a.min_bounds.z; 1.0;
       a.min_bounds.x; a.max_bounds.y; a.min_bounds.z; 1.0 |]
