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
  let raw_data a =
    [| a.min_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f;    // v0;v1;v2;v3 (front)
       a.min_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f;    // v0;v3;v4;v5 (right)
       a.min_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;    // v0;v5;v6;v1 (top)
       a.max_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f;    // v1;v6;v7;v2 (left)
       a.max_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f;    // v7;v4;v3;v2 (bottom)
       a.min_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f; |] // v4;v7;v6;v5 (back)

  // hack to prevent plane overlap (flickering) by shrinking the aabb.
  let fudge a =
    { min_bounds = a.min_bounds + Vector3(0.1f, 0.1f, 0.1f);
      max_bounds = a.max_bounds - Vector3(0.1f, 0.1f, 0.1f); }

  // hard code indices and normals.
  let indices_data = [| 0;  1;  2;   2;  3;  0;       // front
                        4;  5;  6;   6;  7;  4;       // right
                        8;  9; 10;  10; 11;  8;       // top
                       12; 13; 14;  14; 15; 12;       // left
                       16; 17; 18;  18; 19; 16;       // bottom
                       20; 21; 22;  22; 23; 20 |];    // back

  let normal_data = [| 0.0f; 0.0f; 1.0f; 0.0f;
                       0.0f; 0.0f; 1.0f; 0.0f;
                       0.0f; 0.0f; 1.0f; 0.0f;
                       0.0f; 0.0f; 1.0f; 0.0f;    // v0.0f;v1.0f;v2;v3 (front)
                       1.0f; 0.0f; 0.0f; 0.0f;
                       1.0f; 0.0f; 0.0f; 0.0f;
                       1.0f; 0.0f; 0.0f; 0.0f;
                       1.0f; 0.0f; 0.0f; 0.0f;    // v0.0f;v3;v4;v5 (right)
                       0.0f; 1.0f; 0.0f; 0.0f;
                       0.0f; 1.0f; 0.0f; 0.0f;
                       0.0f; 1.0f; 0.0f; 0.0f;
                       0.0f; 1.0f; 0.0f; 0.0f;    // v0.0f;v5;v6;v1 (top)
                       1.0f; 0.0f; 0.0f; 0.0f;
                       -1.0f; 0.0f; 0.0f; 0.0f;
                       -1.0f; 0.0f; 0.0f; 0.0f;
                       -1.0f; 0.0f; 0.0f; 0.0f;    // v1.0f;v6;v7;v2 (left)
                       0.0f; -1.0f; 0.0f; 0.0f;
                       0.0f; -1.0f; 0.0f; 0.0f;
                       0.0f; -1.0f; 0.0f; 0.0f;
                       0.0f; -1.0f; 0.0f; 0.0f;    // v7;v4;v3;v2 (bottom)
                       0.0f; 0.0f; -1.0f; 0.0f;
                       0.0f; 0.0f; -1.0f; 0.0f;
                       0.0f; 0.0f; -1.0f; 0.0f;
                       0.0f; 0.0f; -1.0f; 0.0f |]; // v4;v7;v6;v5 (back)
