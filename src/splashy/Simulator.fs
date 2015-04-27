namespace splashy

open Vector
open Aabb
open Grid

module Simulator =

  // configuration options.
  [<Literal>]
  let max_velocity = 100.0 // for now

  // pre-calculated.
  let get_time_step = Grid.time_step_constant * Grid.h / max_velocity

  let bounds = { min_bounds = Vector3d(-1.0, -1.0, -1.0);
                 max_bounds = Vector3d( 1.0,  1.0,  1.0) }

  // generate a random amount of markers to begin with (testing purposes only)
  let generate n =
    let r = System.Random()
    Grid.markers <- [ for i in 0..n-1 do
                        let x = r.Next(-n, i)
                        let y = r.Next(-i, n)
                        let z = r.Next(-n, n)
                        yield { x = x; y = y; z = z; } ]

  let dynamic_grid_update () =
    Grid.reset ()
    // synchronize fluid markers with the grid
    Seq.iter (fun m ->
              match Grid.get m with
                | Some(c) when not (Grid.is_solid c) ->
                    Grid.set m { c with media = Fluid; layer = Some(0); }
                | None ->
                    if Aabb.contains bounds (Grid.to_vector m) then
                      Grid.add m { Grid.default_cell with media = Fluid; layer = Some(0); }
                | _ -> ()
              ) Grid.markers
    // create air buffer zones around the fluid
    let max_distance = max 2 (int (ceil Grid.time_step_constant))
    for i in 1..max_distance do
      let current_layer = Some(i - 1)
      let current = Grid.filter_values (fun c -> not (Grid.is_solid c) && c.layer = current_layer)
      let all_neighbors = Seq.collect Grid.neighbors current
      Seq.iter (fun where ->
                match Grid.get where with
                  | Some(c) when not (Grid.is_solid c) && c.layer = None ->
                      Grid.set where { c with media = Air; layer = Some(i) }
                  | None ->
                      if Aabb.contains bounds (Grid.to_vector where) then
                        Grid.add where { Grid.default_cell with media = Fluid; layer = Some(i) }
                      else
                        Grid.add where { Grid.default_cell with media = Solid; layer = Some(i) }
                  | _ -> ()
                ) all_neighbors
      ()
    // delete any leftover cells.
    let leftover = Grid.filter_values (fun c -> c.layer = None)
    Seq.iter Grid.delete leftover

  let advance () =
    printfn "Moving simulation forward."
    dynamic_grid_update ()
    ()
