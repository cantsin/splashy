namespace splashy

open OpenTK

/// axis-aligned bounding boxes: used to draw the bounds of the
/// simulation and also those cells that represent fluid markers.
module Aabb =
  type Aabb = { min_bounds: Vector3;
                max_bounds: Vector3 }

  let contains a (p: Vector3) =
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

  // hard code indices and normals.
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
