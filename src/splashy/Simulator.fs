namespace splashy

open MathNet.Numerics.LinearAlgebra

open System.Collections.Generic

open Constants
open Vector
open Aabb
open Coord
open Grid

module Simulator =

  let mutable markers: Coord list = []

  // synchronize our fluid markers with the grid.
  let update_fluid_markers () =
    Seq.iter (fun m ->
                match Grid.get m with
                  | Some c when c.is_not_solid () ->
                      Grid.set m { c with media = Fluid; layer = Some 0; }
                  | None ->
                      if Aabb.contains Constants.bounds (m.to_vector3 ()) then
                        Grid.add m { Grid.default_cell with media = Fluid; layer = Some 0; }
                  | _ -> ()
              ) markers

  // create air buffer zones around the fluid markers.
  let create_air_buffer () =
    for i in 1..Grid.max_distance do
      let previous_layer_count = Some (i - 1)
      let previous_layer = Grid.filter_values (fun c -> c.is_not_solid () && c.layer = previous_layer_count)
      let all_neighbors = Seq.collect (fun (c: Coord) -> c.neighbors ()) previous_layer
      Seq.iter (fun (_, where) ->
                  match Grid.get where with
                    | Some c when c.is_not_solid () && c.layer = None ->
                        Grid.set where { c with media = Air; layer = Some i }
                    | None ->
                        if Aabb.contains Constants.bounds (where.to_vector3 ()) then
                          Grid.add where { Grid.default_cell with media = Air; layer = Some i }
                        else
                          Grid.add where { Grid.default_cell with media = Solid; layer = Some i }
                    | _ -> ()
                ) all_neighbors

  // convection by means of a backwards particle trace.
  let apply_convection dt =
    let new_velocities = Seq.map (fun m ->
                                    let new_position = trace m dt
                                    match Grid.get new_position with
                                      | Some c -> c.velocity
                                      | _ -> failwith "Backwards particle trace went too far."
                                  ) markers
    Seq.iter2 (fun m new_v ->
                 let c = Grid.raw_get m
                 Grid.set m { c with velocity = new_v }
              ) markers new_velocities

  // gravity only (for now).
  let apply_forces dt =
    let v = Constants.gravity .* dt
    Seq.iter (fun (m: Coord) ->
                for direction, neighbor in m.neighbors () do
                  match Grid.get neighbor with
                    | Some c ->
                      if c.media = Fluid && Coord.is_bordering direction v then
                        Grid.set neighbor { c with velocity = c.velocity .+ v }
                    | _ ->
                      if Aabb.contains Constants.bounds (neighbor.to_vector3 ()) then
                        failwith <| sprintf "Could not get neighbor %O of %O." neighbor m
              ) markers

  // viscosity by evaluating the lapalacian on bordering fluid cells.
  let apply_viscosity dt =
    let const_v = Constants.fluid_viscosity * dt
    let velocities = Seq.map Grid.laplacian markers
    Seq.iter2 (fun m v ->
                 let c = Grid.raw_get m
                 Grid.set m { c with velocity = c.velocity .+ (v .* const_v) }
               ) markers velocities

  // set the pressure such that the divergence throughout the fluid is zero.
  let apply_pressure dt =
    let n = Seq.length markers
    let lookups = Seq.zip markers { 0..n } |> dict
    let coefficients (c: Coord) =
      let neighbors = c.neighbors ()
      // return a 1 for every bordering liquid marker.
      let singulars = Seq.fold (fun accum (_, n) ->
                                  if lookups.ContainsKey n then
                                    (lookups.[n], 1.0) :: accum
                                  else
                                    accum
                                ) [] neighbors
      // return -N for this marker, where N is number of non solid neighbors.
      let N = Grid.number_neighbors (fun n -> n.is_not_solid ()) c
      (lookups.[c], - N) :: singulars
    // construct a sparse matrix of coefficients.
    let mutable m = SparseMatrix.zero<float> n n
    for KeyValue(marker, c) in lookups do
      for (r, value) in coefficients marker do
        m.[r, c] <- value
    // construct divergence of velocity field.
    let c = Constants.h * Constants.fluid_density / dt
    let b = Seq.map (fun m ->
                       let f = Grid.divergence m
                       let a = Grid.number_neighbors (fun c -> c.media = Air) m
                       let s = Constants.atmospheric_pressure
                       c * f - s * a
                     ) markers
                     |> Seq.toList |> vector
    let results = m.Solve(b)
    // apply pressure only to borders of fluid cells.
    let inv_c = 1.0 / c
    let gradient (where: Coord) v =
      let neighbors = where.forwardNeighbors ()
      let p = results.[lookups.[where]]
      let (_, v1) = neighbors.[0]
      let (_, v2) = neighbors.[1]
      let (_, v3) = neighbors.[2]
      let n1 = if lookups.ContainsKey v1 then results.[lookups.[v1]] else 1.0
      let n2 = if lookups.ContainsKey v2 then results.[lookups.[v2]] else 1.0
      let n3 = if lookups.ContainsKey v3 then results.[lookups.[v3]] else 1.0
      Vector3d(p - n1, p - n2, p - n3)
    Seq.iter2 (fun m result ->
                 let pressure = gradient m results
                 let c = Grid.raw_get m
                 Grid.set m { c with velocity = c.velocity .- (pressure .* inv_c) }
               ) markers results


  // propagate the fluid velocities into the buffer zone.
  let propagate_velocities () =
    for i in 1..max_distance do
      let nonfluid = Grid.filter_values (fun c -> c.layer = None)
      let previous_layer_count = Some (i - 1)
      let get_previous_layer (_, c) = match Grid.get c with
                                        | Some c when c.layer = previous_layer_count -> true
                                        | _ -> false
      Seq.iter (fun (m: Coord) ->
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
                ) nonfluid

  // zero out any velocities that go into solid cells.
  let zero_solid_velocities () =
    let solids = Grid.filter_values (fun c -> c.is_solid ())
    Seq.iter (fun (m: Coord) ->
                let neighbors = m.neighbors ()
                for (dir, neighbor) in neighbors do
                  let inward = Coord.reverse dir
                  match Grid.get neighbor with
                    | Some n when n.is_not_solid () ->
                      Grid.set neighbor { n with velocity = Coord.merge inward n.velocity Vector3d.ZERO }
                    | _ -> ()
              ) solids

  let move_markers dt =
    // for now, advance by frame.
    markers <- Seq.map (fun (marker: Coord) ->
                          let p = marker.to_vector3d ()
                          let c = Grid.raw_get marker
                          let new_coords = p .+ (c.velocity .* dt)
                          { x = int new_coords.x; y = int new_coords.y; z = int new_coords.z; }
                        ) markers |> Seq.toList

  let advance () =
    printfn "Moving simulation forward with time step %A." Constants.time_step
    let dt = Constants.time_step
    Grid.setup (fun () ->
      printfn "  Setup: Updating fluid markers."
      update_fluid_markers ()
      printfn "  Setup: Creating air buffer."
      create_air_buffer ()
    )
    printfn "Applying convection."
    apply_convection dt // -(∇⋅u)u
    printfn "Applying forces."
    apply_forces dt     // F
    printfn "Applying viscosity."
    apply_viscosity dt  // v∇²u
    printfn "Applying pressure."
    apply_pressure dt   // -1/ρ∇p
    printfn "Cleaning up grid."
    Grid.cleanup (fun () ->
      printfn "  Cleanup: Propagating fluid velocities into surroundings."
      propagate_velocities ()
      printfn "  Cleanup: Setting solid cell velocities to zero."
      zero_solid_velocities()
    )
    printfn "Moving fluid markers."
    move_markers dt
    // sanity check.
    Seq.iter (fun (m: Coord) ->
                if not (Aabb.contains Constants.bounds (m.to_vector3 ())) then
                  failwith (sprintf "Error: Fluid markers went outside bounds (example: %O)." m)
             ) markers

  // generate a random amount of markers to begin with (testing purposes only)
  let generate n =
    let r = System.Random()
    let h = int Constants.h
    let l = 2
    let l = int (Constants.bounds_h / float32 Constants.h)
    let new_markers = [ for _ in 0..n-1 do
                        let x = r.Next(-l, l + 1) * h
                        let y = r.Next(-l, l + 1) * h
                        let z = r.Next(-l, l + 1) * h
                        yield { x = x; y = y; z = z; } ]
    markers <- Set.ofList new_markers |> Seq.toList
    update_fluid_markers ()
