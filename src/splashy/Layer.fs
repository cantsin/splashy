namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic

open Cell
open Vector
open Coord
open Grid
open World

module Layer =

  let max_distance = Operators.max 2 (int (ceil Constants.time_step_constant))

  let mutable private layers = new Dictionary<Coord, Option<int>>() // used for constructing buffer zones.

  let setup fn =
    try
      // reset grid layers.
      let coords = Grid.filter (fun _ -> true)
      Seq.iter (fun m ->
                  let c = raw_get m
                  set m { c with layer = None }
                ) coords
      fn ()
    finally
      // get rid of unused layers.
      let leftover = Grid.filter (fun c -> c.layer = None)
      Seq.iter delete leftover

  let cleanup fn =
    // reset grid layers but mark fluids as layer 0.
    let coords = Grid.filter (fun _ -> true)
    Seq.iter (fun m ->
                let c = raw_get m
                set m { c with layer = if c.media = Fluid then Some 0 else None }
              ) coords
    fn ()

  // create air buffer zones around the fluid markers.
  let create_air_buffer () =
    for i in 1..max_distance do
      let previous_layer_count = Some (i - 1)
      let previous_layer = Grid.filter (fun c -> c.is_not_solid () && c.layer = previous_layer_count)
      let all_neighbors = Seq.collect (fun (c: Coord) -> c.neighbors ()) previous_layer
      for (_, where) in all_neighbors do
        match Grid.get where with
          | Some c when c.media <> Fluid && c.layer = None ->
            Grid.set where { c with layer = Some i }
          | None ->
            if Aabb.contains World.bounds where then
              Grid.add where { Cell.default_cell with media = Air;
                                                      layer = Some i;
                                                      pressure = Some Constants.atmospheric_pressure; }
            else
              Grid.add where { Cell.default_cell with media = Solid;
                                                      layer = Some i }
          | _ -> ()

  // propagate the fluid velocities into the buffer zone.
  let propagate_velocities () =
    for i in 1..max_distance do
      let nonfluid = Grid.filter (fun c -> c.layer = None)
      let previous_layer_count = Some (i - 1)
      let get_previous_layer (_, c) = match Grid.get c with
                                        | Some c when c.layer = previous_layer_count -> true
                                        | _ -> false
      for m in nonfluid do
        let neighbors = m.neighbors ()
        let previous_layer = Seq.filter get_previous_layer neighbors
        let n = Seq.length previous_layer
        if n <> 0 then
          let c = Grid.raw_get m
          let velocities = Seq.map (fun (_, where) -> (Grid.raw_get where).velocity) previous_layer
          let pla = Vector.average velocities
          for dir, neighbor in neighbors do
            match Grid.get neighbor with
              | Some n when n.media <> Fluid && Coord.is_bordering dir c.velocity ->
                let new_v = Coord.merge dir c.velocity pla
                set m { c with velocity = new_v }
              | _ -> ()
          set m { c with layer = previous_layer_count }

  // zero out any velocities that go into solid cells.
  let zero_solid_velocities () =
    let solids = Grid.filter (fun c -> c.is_solid ())
    for solid in solids do
      let neighbors = solid.neighbors ()
      for (dir, neighbor) in neighbors do
        let inward = Coord.reverse dir
        match Grid.get neighbor with
          | Some n when n.is_not_solid () && Coord.is_bordering inward n.velocity ->
            Grid.set neighbor { n with velocity = Coord.merge inward n.velocity Vector3d.ZERO }
          | _ -> ()

  // synchronize our fluid markers with the grid.
  let sync_markers markers =
    for marker in markers do
      match Grid.get marker with
        | Some c when c.is_not_solid () ->
          Grid.set marker { c with media = Fluid; layer = Some 0; }
        | None ->
          if Aabb.contains World.bounds marker then
            Grid.add marker { Cell.default_cell with media = Fluid; layer = Some 0; }
        | _ ->
          ()

  let update_velocities (velocities: seq<Coord * Vector3d<m/s>>) =
    Seq.iter (fun (where, new_v) ->
                let c = Grid.raw_get where
                Grid.set where { c with velocity = new_v }
              ) velocities

  let update_pressures (pressures: seq<Coord * float<kg/(m*s^2)>>) =
    Seq.iter (fun (where, new_p) ->
                let c = Grid.raw_get where
                Grid.set where { c with pressure = Some new_p }
              ) pressures

  let check_containment markers =
    Seq.iter (fun (m: Coord) ->
                if not (Aabb.contains World.bounds m) then
                  failwith (sprintf "Error: Fluid marker went outside bounds (example: %O)." m)
             ) markers
