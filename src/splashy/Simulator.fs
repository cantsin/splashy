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
                | Some c when not (Grid.is_solid c) ->
                    Grid.set m { c with media = Fluid; layer = Some 0; }
                | None ->
                    if Aabb.contains Constants.bounds (m.to_vector ()) then
                      Grid.add m { Grid.default_cell with media = Fluid; layer = Some 0; }
                | _ -> ()
              ) markers

  // create air buffer zones around the fluid markers.
  let create_air_buffer () =
    for i in 1..Grid.max_distance do
      let current_layer = Some (i - 1)
      let current = Grid.filter_values (fun c -> not (Grid.is_solid c) && c.layer = current_layer)
      let all_neighbors = Seq.collect (fun (c: Coord) -> c.neighbors ()) current
      Seq.iter (fun (_, where) ->
                match Grid.get where with
                  | Some c when not (Grid.is_solid c) && c.layer = None ->
                      Grid.set where { c with media = Air; layer = Some i }
                  | None ->
                      if Aabb.contains Constants.bounds (where.to_vector ()) then
                        Grid.add where { Grid.default_cell with media = Air; layer = Some i }
                      else
                        Grid.add where { Grid.default_cell with media = Solid; layer = Some i }
                  | _ -> ()
                ) all_neighbors

  // convection by means of a backwards particle trace.
  let apply_convection () =
    let new_positions = Seq.map (fun m -> trace m Constants.time_step) markers
    Seq.iter2 (fun m np ->
               match Grid.get m, Grid.get np with
                 | Some c1, Some c2 -> Grid.set m { c1 with velocity = c2.velocity }
                 | _ -> failwith "Backwards particle trace went too far."
               ) markers new_positions

  // gravity only (for now).
  let apply_forces () =
    let v = Constants.gravity .* Constants.time_step
    Seq.iter (fun (m: Coord) ->
              for direction, neighbor in m.neighbors () do
                match Grid.get neighbor with
                  | Some c ->
                    if c.media = Fluid && Coord.is_bordering direction v then
                      Grid.set neighbor { c with velocity = c.velocity .+ v }
                  | None -> failwith "Could not get neighbor."
              ) markers

  // viscosity by evaluating the lapalacian on bordering fluid cells.
  let apply_viscosity () =
    let v = Constants.fluid_viscosity * Constants.time_step
    let velocities = Seq.map Grid.laplacian markers
    Seq.iter2 (fun m velocity ->
               match Grid.get m with
                 | Some c -> Grid.set m { c with velocity = c.velocity .+ (velocity .* v) }
                 | None -> failwith "Marker did not have grid."
               ) markers velocities

  // set the pressure such that the divergence throughout the fluid is zero.
  let apply_pressure () =
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
      let N = Seq.filter (fun (_, n) ->
                          match Grid.get n with
                            | Some c -> not (Grid.is_solid c)
                            | None -> false) neighbors
                          |> Seq.length
      (lookups.[c], - float N) :: singulars
    // construct a sparse matrix of coefficients.
    let mutable m = SparseMatrix.zero<float> n n
    for KeyValue(marker, c) in lookups do
      for (r, value) in coefficients marker do
        m.[r, c] <- value
    // construct divergence of velocity field
    let c = Constants.h * Constants.fluid_density / Constants.time_step
    let air (c: Coord) =
      let neighbors = c.neighbors ()
      Seq.filter(fun (_, n) ->
                 match Grid.get n with
                   | Some c -> c.media = Air
                   | None -> false) neighbors
                   |> Seq.length |> float
    let b = Seq.map (fun m ->
                     let f = Grid.divergence m
                     let s = Constants.atmospheric_pressure * air m
                     c * f - s
                     ) markers
                     |> Seq.toList |> vector
    let results = m.Solve(b)
    // finally apply pressure (but only to borders of fluid cells).
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
               match Grid.get m with
                 | Some c -> Grid.set m { c with velocity = c.velocity .- (pressure .* inv_c) }
                 | None -> failwith "Marker did not have grid."
               ) markers results

  let advance () =
    printfn "Moving simulation forward with time step %A." Constants.time_step
    Grid.setup (fun () ->
      update_fluid_markers ()
      create_air_buffer ()
    )
    apply_convection () // -(∇⋅u)u
    apply_forces ()     // F
    apply_viscosity ()  // v∇²u
    apply_pressure()    // -1/ρ∇p
    Grid.cleanup ()

  // generate a random amount of markers to begin with (testing purposes only)
  let generate n =
    let r = System.Random()
    let l = int Constants.h
    markers <- [ for _ in 0..n-1 do
                 let x = r.Next(-l, l)
                 let y = r.Next(-l, l)
                 let z = r.Next(-l, l)
                 yield { x = x; y = y; z = z; } ]
    update_fluid_markers ()
