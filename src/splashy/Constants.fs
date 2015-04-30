namespace splashy

open Vector
open Aabb

module Constants =

  // configuration options.

  [<Literal>]
  let h = 10.0
  [<Literal>]
  let viscosity = 0.894
  [<Literal>]
  let max_velocity = 100.0 // arbitrary!

  let gravity = Vector3d(0.0, -9.81, 0.0) // m/s^2

  [<Literal>]
  let time_step_constant = 2.5
  // pre-calculated.
  let time_step = time_step_constant * h / max_velocity

  // for now.
  let bounds_h = 100.0
  let bounds = { min_bounds = Vector3d(-bounds_h, -bounds_h, -bounds_h);
                 max_bounds = Vector3d( bounds_h,  bounds_h,  bounds_h) }
