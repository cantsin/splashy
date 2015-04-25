namespace splashy

open Vector
open Grid

module Simulator =

  // configuration options.
  [<Literal>]
  let max_velocity = 100.0 // for now

  // pre-calculated.
  let get_time_step = time_step_constant * h / max_velocity

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
