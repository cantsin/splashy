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

  // for rendering purposes.
  let rawData a =
    [| a.min_bounds.x; a.min_bounds.y; a.max_bounds.z; 1.0;
       a.max_bounds.x; a.min_bounds.y; a.max_bounds.z; 1.0;
       a.max_bounds.x; a.max_bounds.y; a.max_bounds.z; 1.0;
       a.min_bounds.x; a.max_bounds.y; a.max_bounds.z; 1.0;
       a.min_bounds.x; a.min_bounds.y; a.min_bounds.z; 1.0;
       a.max_bounds.x; a.min_bounds.y; a.min_bounds.z; 1.0;
       a.max_bounds.x; a.max_bounds.y; a.min_bounds.z; 1.0;
       a.min_bounds.x; a.max_bounds.y; a.min_bounds.z; 1.0 |]

  let indicesData = [| 0; 3; 2; 1;    // front face
                       3; 2; 6; 7;    // top face
                       7; 6; 5; 4;    // back face
                       4; 0; 3; 7;    // left face
                       4; 5; 1; 0;    // bottom face
                       1; 5; 6; 2; |] // right face

  let normalData = [| -1.0f; -1.0f;  1.0f; 0.0f;
                       1.0f; -1.0f;  1.0f; 0.0f;
                       1.0f;  1.0f;  1.0f; 0.0f;
                      -1.0f;  1.0f;  1.0f; 0.0f;
                      -1.0f; -1.0f; -1.0f; 0.0f;
                       1.0f; -1.0f; -1.0f; 0.0f;
                       1.0f;  1.0f; -1.0f; 0.0f;
                      -1.0f;  1.0f; -1.0f; 0.0f; |]
