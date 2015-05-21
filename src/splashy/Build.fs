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

    let has_value c l = layers.[c] = l

    let filter (fn: Option<int> -> bool) =
      // make a copy: we want to avoid writing to the dictionary while potentially iterating over it.
      Seq.filter (fun (KeyValue(k, v)) -> fn v) layers |>
      Seq.map (fun (KeyValue(k, v)) -> k) |>
      fun keys -> new List<Coord> (keys)

    let delete c = ignore <| layers.Remove c

    let clear () = layers.Clear ()

  let max_distance = Operators.max 2 (int (ceil Constants.time_step_constant))

  // reset grid layers.
  let setup fn =
    Layers.clear ()
    Grid.filter (fun _ -> true) |> Seq.iter (fun m -> Layers.set m None)
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
    Seq.iter Grid.delete leftover
    Seq.iter Layers.delete leftover

  // create air buffer zones around the fluid markers.
  let create_air_buffer () =
    for i in 1..max_distance do
      let all_neighbors =
        Grid.filter Cell.media_is_not_solid |>
        Seq.filter (fun c -> Layers.has_value c (Some (i - 1))) |>
        Seq.collect (fun c -> c.neighbors ())
      for (_, where) in all_neighbors do
        match Grid.get where with
          | Some c when c.media <> Fluid && Layers.has_value where None ->
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
    let mutable result = []
    for i in 1..max_distance do
      let nonfluid = Layers.filter (fun v -> v = None)
      let get_previous_layer (_, neighbor) =
        match Grid.get neighbor with
          | Some c when Layers.has_value neighbor (Some (i - 1)) -> true
          | _ -> false
      for m in nonfluid do
        let neighbors = m.neighbors ()
        let previous_layer = Seq.filter get_previous_layer neighbors
        let n = Seq.length previous_layer
        if n <> 0 then
          let c = Grid.raw_get m
          for dir, neighbor in neighbors do
            match Grid.get neighbor with
              | Some n when n.media <> Fluid && Coord.is_bordering dir c.velocity ->
                let velocities = Seq.map (fun (_, where) -> (Grid.raw_get where).velocity) previous_layer
                let pla = Vector.average velocities
                let new_v = Coord.merge dir c.velocity pla
                result <- (m, new_v) :: result
              | _ -> ()
          Layers.set m (Some (i - 1))
    result

  // zero out any velocities that go into solid cells.
  let zero_solid_velocities () =
    let solids = Grid.filter Cell.media_is_solid
    let has_inward_velocity (dir, neighbor) =
      let inward = Coord.reverse dir
      match Grid.get neighbor with
        | Some n when n.is_not_solid () && Coord.is_bordering inward n.velocity -> true
        | _ -> false
    Seq.collect (fun (solid: Coord) ->
                   let neighbors = solid.neighbors ()
                   let filtered = Seq.filter has_inward_velocity neighbors
                   Seq.map (fun (dir, neighbor) ->
                              let inward = Coord.reverse dir
                              let n = Grid.raw_get neighbor
                              let new_v = Coord.merge inward n.velocity Vector3d.ZERO
                              (neighbor, new_v)
                           ) filtered
                 ) solids

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
