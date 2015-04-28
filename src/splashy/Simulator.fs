namespace splashy

open Vector
open Aabb
open Grid

module Simulator =

  // configuration options.
  [<Literal>]
  let max_velocity = 100.0 // for now

  // pre-calculated.
  let time_step = Grid.time_step_constant * Grid.h / max_velocity

  let h = 100.0
  let bounds = { min_bounds = Vector3d(-h, -h, -h);
                 max_bounds = Vector3d( h,  h,  h) }

  let mutable markers: Coord list = []

  // synchronize fluid markers with the grid
  let update_fluid_markers () =
    Seq.iter (fun m ->
              match Grid.get m with
                | Some(c) when not (Grid.is_solid c) ->
                    Grid.set m { c with media = Fluid; layer = Some(0); }
                | None ->
                    if Aabb.contains bounds (m.to_vector ()) then
                      Grid.add m { Grid.default_cell with media = Fluid; layer = Some(0); }
                | _ -> ()
              ) markers

  // create air buffer zones around the fluid
  let create_air_buffer () =
    let max_distance = max 2 (int (ceil Grid.time_step_constant))
    for i in 1..max_distance do
      let current_layer = Some(i - 1)
      let current = Grid.filter_values (fun c -> not (Grid.is_solid c) && c.layer = current_layer)
      let all_neighbors = Seq.collect (fun (c: Coord) -> c.neighbors ()) current
      Seq.iter (fun where ->
                match Grid.get where with
                  | Some(c) when not (Grid.is_solid c) && c.layer = None ->
                      Grid.set where { c with media = Air; layer = Some(i) }
                  | None ->
                      if Aabb.contains bounds (where.to_vector ()) then
                        Grid.add where { Grid.default_cell with media = Air; layer = Some(i) }
                      else
                        Grid.add where { Grid.default_cell with media = Solid; layer = Some(i) }
                  | _ -> ()
                ) all_neighbors

  // delete any leftover cells.
  let excise_unused_grid_cells () =
    let leftover = Grid.filter_values (fun c -> c.layer = None)
    Seq.iter Grid.delete leftover

  let convection () =
    Seq.iter (fun m ->
      let new_velocity = trace m time_step
      printfn "%A" new_velocity) markers

  let advance () =
    printfn "Moving simulation forward."
    Grid.reset ()
    update_fluid_markers ()
    create_air_buffer ()
    excise_unused_grid_cells ()
    convection ()

  // generate a random amount of markers to begin with (testing purposes only)
  let generate n =
    let r = System.Random()
    let l = int h
    markers <- [ for _ in 0..n-1 do
                 let x = r.Next(-l, l)
                 let y = r.Next(-l, l)
                 let z = r.Next(-l, l)
                 yield { x = x; y = y; z = z; } ]
    update_fluid_markers ()
