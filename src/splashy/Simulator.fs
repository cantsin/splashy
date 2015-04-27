namespace splashy

open Vector
open Aabb
open Grid

module Simulator =

  // configuration options.
  [<Literal>]
  let max_velocity = 100.0 // for now

  // pre-calculated.
  let get_time_step = time_step_constant * h / max_velocity

  let bounds = { min_bounds = Vector3d(-1.0, -1.0, -1.0);
                 max_bounds = Vector3d( 1.0,  1.0,  1.0) }

  let dynamic_grid_update () =
    Grid.reset ()
    Seq.iter (fun m ->
              match Grid.get m with
                | Some(c) ->
                  match c.media with
                    | Solid -> () // do nothing
                    | _ -> Grid.set m { c with media = Fluid; layer = 0; }
                | None ->
                  Grid.add m { pressure = 0.0; media = Fluid; layer = 0; velocity = Vector3d() }
                  printf "%A" m
              ) Grid.markers

  let advance () = ()
