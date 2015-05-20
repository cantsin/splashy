namespace splashy

open OpenTK

open Constants
open Aabb

module World =

  // the world bounding box.
  let bounds = { min_bounds = Vector3(-Constants.world_h, -Constants.world_h, -Constants.world_h);
                 max_bounds = Vector3( Constants.world_h,  Constants.world_h,  Constants.world_h) }
