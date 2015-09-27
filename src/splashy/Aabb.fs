namespace Splashy

open OpenTK

open Coord

/// axis-aligned bounding boxes: used to draw the bounds of the
/// simulation and also those cells that represent fluid markers.
module Aabb =
  type Aabb = { min_bounds: Vector3;
                max_bounds: Vector3 }

  let contains a (c: Coord) =
    let p = Vector3(float32 c.x, float32 c.y, float32 c.z)
    p.[0] >= a.min_bounds.[0] && p.[0] <= a.max_bounds.[0] &&
    p.[1] >= a.min_bounds.[1] && p.[1] <= a.max_bounds.[1] &&
    p.[2] >= a.min_bounds.[2] && p.[2] <= a.max_bounds.[2]

  // for rendering purposes.
  // NB: vertices are counted counter-clockwise.
  let raw_data a =
    [|
     a.min_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f; // 1
     a.max_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f; // 2
     a.max_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f; // 3
     a.min_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f; // 4
     a.min_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f; // 5
     a.max_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f; // 6
     a.max_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f; // 7
     a.min_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f; // 8
     |]

  // hack to prevent plane overlap (flickering) by shrinking the aabb.
  let fudge a =
    { min_bounds = a.min_bounds + Vector3(0.1f, 0.1f, 0.1f);
      max_bounds = a.max_bounds - Vector3(0.1f, 0.1f, 0.1f); }

  // hard code indices and normals.
  let indices_data = [|
                      0; 1; 2;  2; 3; 0; // front
                      3; 2; 6;  6; 7; 3; // top
                      7; 6; 5;  5; 4; 7; // back
                      4; 0; 3;  3; 7; 4; // left
                      4; 5; 1;  1; 0; 4; // bottom
                      1; 5; 6;  6; 2; 1; // right
                      |]

  let normal_data = [|
                     -1.0f; -1.0f;  1.0f; 0.0f;
                      1.0f; -1.0f;  1.0f; 0.0f;
                      1.0f;  1.0f;  1.0f; 0.0f;
                     -1.0f;  1.0f;  1.0f; 0.0f;
                     -1.0f; -1.0f; -1.0f; 0.0f;
                      1.0f; -1.0f; -1.0f; 0.0f;
                      1.0f;  1.0f; -1.0f; 0.0f;
                     -1.0f;  1.0f; -1.0f; 0.0f;
                     |]
