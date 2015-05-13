namespace splashy

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
    [| a.min_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.min_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.max_bounds.[1]; a.max_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.min_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.max_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f;
       a.min_bounds.[0]; a.max_bounds.[1]; a.min_bounds.[2]; 1.0f |]

  // hack to prevent plane overlap (flickering) by shrinking the aabb.
  let fudge a =
    { min_bounds = a.min_bounds + Vector3(0.1f, 0.1f, 0.1f);
      max_bounds = a.max_bounds - Vector3(0.1f, 0.1f, 0.1f); }

  // hard code indices and normals.
  let indices_data = [| 0; 3; 2; 1;    // front face
                        3; 2; 6; 7;    // top face
                        7; 6; 5; 4;    // back face
                        4; 0; 3; 7;    // left face
                        4; 5; 1; 0;    // bottom face
                        1; 5; 6; 2; |] // right face

  let normal_data = [| -1.0f; -1.0f;  1.0f; 0.0f;
                        1.0f; -1.0f;  1.0f; 0.0f;
                        1.0f;  1.0f;  1.0f; 0.0f;
                       -1.0f;  1.0f;  1.0f; 0.0f;
                       -1.0f; -1.0f; -1.0f; 0.0f;
                        1.0f; -1.0f; -1.0f; 0.0f;
                        1.0f;  1.0f; -1.0f; 0.0f;
                       -1.0f;  1.0f; -1.0f; 0.0f; |]
