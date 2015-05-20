namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic

open Cell
open Vector
open Coord
open Grid
open World

module Build =

  // handle the creation of layers (buffer zones).
  module private Layers =

    let mutable private layers = new Dictionary<Coord, Option<int>>()

    let get c = layers.[c]

    let set c l = ignore <| layers.[c] <- l

    let filter (fn: Option<int> -> bool) =
      let keys = Seq.filter (fun (KeyValue(k, v)) -> fn v) layers |> Seq.map (fun (KeyValue(k, v)) -> k)
      new List<Coord> (keys)

    let delete c = ignore <| layers.Remove c

    let clear () = layers.Clear ()

  let max_distance = Operators.max 2 (int (ceil Constants.time_step_constant))

  let setup fn =
    // reset grid layers.
    Layers.clear ()
    let coords = Grid.filter (fun _ -> true)
    Seq.iter (fun m ->
                Layers.set m None
              ) coords
    fn ()

  // synchronize our fluid markers with the grid.
  let sync_markers markers =
    for marker in markers do
      match Grid.get marker with
        | Some c when c.is_not_solid () ->
          Grid.set marker { c with media = Fluid; }
          Layers.set marker (Some 0)
        | None ->
          if World.contains marker then
            Grid.add marker { media = Fluid;
                              velocity = Vector3d.ZERO;
                              pressure = None; }
            Layers.set marker (Some 0)
        | _ ->
          ()

  // reset grid layers but mark fluids as layer 0.
  let cleanup fn =
    let coords = Grid.filter (fun _ -> true)
    Seq.iter (fun m ->
                let c = Grid.raw_get m
                Layers.set m (if c.media = Fluid then Some 0 else None)
              ) coords
    fn ()

  let delete_unused () =
    let leftover = Layers.filter (fun v -> v = None)
    Seq.iter delete leftover
    Seq.iter Layers.delete leftover

  // create air buffer zones around the fluid markers.
  let create_air_buffer () =
    for i in 1..max_distance do
      let previous_layer_count = Some (i - 1)
      let nonsolids = Grid.filter (fun c -> c.is_not_solid ())
      let previous_layer = Seq.filter (fun c -> (Layers.get c) = previous_layer_count) nonsolids
      let all_neighbors = Seq.collect (fun (c: Coord) -> c.neighbors ()) previous_layer
      for (_, where) in all_neighbors do
        match Grid.get where with
          | Some c when c.media <> Fluid && ((Layers.get where) = None) ->
            Layers.set where (Some i)
          | None ->
            if World.contains where then
              Grid.add where { media = Air;
                               velocity = Vector3d.ZERO;
                               pressure = Some Constants.atmospheric_pressure; }
            else
              Grid.add where { media = Solid;
                               velocity = Vector3d.ZERO;
                               pressure = None; }
            Layers.set where (Some i)
          | _ -> ()

  // propagate the fluid velocities into the buffer zone.
  let propagate_velocities () =
    for i in 1..max_distance do
      let nonfluid = Layers.filter (fun v -> v = None)
      let previous_layer_count = Some (i - 1)
      let get_previous_layer (_, n) = match Grid.get n with
                                        | Some c when Layers.get n <> previous_layer_count -> true
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
                Grid.set m { c with velocity = new_v }
              | _ -> ()
          Layers.set m previous_layer_count

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
                if not (World.contains m) then
                  failwith (sprintf "Error: Fluid marker went outside bounds (example: %O)." m)
             ) markers
