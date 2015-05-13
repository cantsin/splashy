namespace splashy

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic
open OpenTK

open Constants
open Vector
open Aabb
open Coord
open Grid

module Simulator =

  // store information about the water particles: their integer coordinates and floating positions.
  let mutable markers: Coord list = []
  let mutable locations: Vector3d<m> list = []

  // the world bounding box.
  let world = { min_bounds = Vector3(-Constants.world_h, -Constants.world_h, -Constants.world_h);
                max_bounds = Vector3( Constants.world_h,  Constants.world_h,  Constants.world_h) }

  // synchronize our fluid markers with the grid.
  let update_fluid_markers () =
    for marker in markers do
      match Grid.get marker with
        | Some c when c.is_not_solid () ->
          Grid.set marker { c with media = Fluid; layer = Some 0; }
        | None ->
          if Aabb.contains world marker then
            Grid.add marker { Grid.default_cell with media = Fluid; layer = Some 0; }
        | _ -> ()

  // create air buffer zones around the fluid markers.
  let create_air_buffer () =
    for i in 1..Grid.max_distance do
      let previous_layer_count = Some (i - 1)
      let previous_layer = Grid.filter_values (fun c -> c.is_not_solid () && c.layer = previous_layer_count)
      let all_neighbors = Seq.collect (fun (c: Coord) -> c.neighbors ()) previous_layer
      for (_, where) in all_neighbors do
        match Grid.get where with
          | Some c when c.is_not_solid () && c.layer = None ->
            Grid.set where { c with media = Air; layer = Some i }
          | None ->
            if Aabb.contains world where then
              Grid.add where { Grid.default_cell with media = Air; layer = Some i }
            else
              Grid.add where { Grid.default_cell with media = Solid; layer = Some i }
          | _ -> ()

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
    let f = Constants.gravity .* dt
    for marker in markers do
      for direction, neighbor in marker.neighbors () do
        let inward = Coord.reverse direction
        match Grid.get neighbor with
          | Some c ->
            if Coord.is_bordering inward f then
              Grid.set marker { c with velocity = c.velocity .+ f }
          | _ ->
            if Aabb.contains world neighbor then
              failwith <| sprintf "Could not get neighbor %O of %O." neighbor marker

  // viscosity by evaluating the lapalacian on bordering fluid cells.
  let apply_viscosity dt =
    let const_v: float<m^2> = Constants.fluid_viscosity * dt
    let velocities = Seq.map Grid.laplacian markers
    Seq.iter2 (fun m result ->
                 let c = Grid.raw_get m
                 let dv = result .* const_v
                 Grid.set m { c with velocity = c.velocity .+ dv }
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
                       let result: float<kg/(m*s^2)> = c * f - (s * a) // ensure pressure units.
                       float result
                     ) markers
                     |> Seq.toList |> vector
    let results = m.Solve(b)
    // apply pressure only to borders of fluid cells.
    let inv_c = 1.0 / c
    let get_result key =
      if lookups.ContainsKey key then
        results.[lookups.[key]] * 1.0<kg/(m*s^2)>
      else
        Constants.atmospheric_pressure
    let gradient (where: Coord) =
      let p = results.[lookups.[where]] * 1.0<kg/(m*s^2)>
      let neighbors = where.forward_neighbors ()
      let (_, v1) = neighbors.[0]
      let (_, v2) = neighbors.[1]
      let (_, v3) = neighbors.[2]
      let n1 = get_result v1
      let n2 = get_result v2
      let n3 = get_result v3
      Vector3d<kg/(m*s^2)>(p - n1, p - n2, p - n3)
    Seq.iter (fun m ->
                let c = Grid.raw_get m
                let pressure = gradient m
                Grid.set m { c with velocity = c.velocity .- (pressure .* inv_c) }
              ) markers

  // propagate the fluid velocities into the buffer zone.
  let propagate_velocities () =
    for i in 1..max_distance do
      let nonfluid = Grid.filter_values (fun c -> c.layer = None)
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
    let solids = Grid.filter_values (fun c -> c.is_solid ())
    for solid in solids do
      let neighbors = solid.neighbors ()
      for (dir, neighbor) in neighbors do
        let inward = Coord.reverse dir
        match Grid.get neighbor with
          | Some n when n.is_not_solid () ->
            Grid.set neighbor { n with velocity = Coord.merge inward n.velocity Vector3d.ZERO }
          | _ -> ()

  let move_markers dt =
    // for now, advance by frame.
    locations <- Seq.map (fun (l: Vector3d<m>) ->
                            let m = Coord.construct(l.x, l.y, l.z)
                            let c = Grid.raw_get m
                            l .+ (c.velocity .* dt)
                          ) locations |> Seq.toList
    markers <- Seq.map (fun (l: Vector3d<m>) -> Coord.construct(l.x, l.y, l.z)) locations |> Seq.toList

  let advance dt =
    let dt = dt * Constants.time_step
    printfn "Moving simulation forward with time step %A." dt
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
                if not (Aabb.contains world m) then
                  failwith (sprintf "Error: Fluid markers went outside bounds (example: %O)." m)
             ) markers

  // generate a random amount of markers to begin with (testing purposes only).
  let generate n =
    let r = System.Random()
    let h = int Constants.h
    let l = int (Constants.world_h / float32 Constants.h)
    let new_markers = [ for _ in 0..n-1 do
                        let x = r.Next(-l, l + 1) * h
                        let y = r.Next(-l, l + 1) * h
                        let z = r.Next(-l, l + 1) * h
                        yield Coord.construct(x, y, z) ]
    markers <- Set.ofList new_markers |> Seq.toList
    let to_vec m = Vector3d<m>(float m.x * 1.0<m>, float m.y * 1.0<m>, float m.z * 1.0<m>)
    locations <- Seq.map to_vec markers |> Seq.toList
    update_fluid_markers ()
