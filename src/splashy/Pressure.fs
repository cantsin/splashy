namespace splashy

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

open Cell
open Vector
open Coord
open Grid

module Pressure =

  // THIS IS WRONG argh
  let internal get_velocities (filter: Cell -> bool) (where: Coord) =
    let neighbors = where.neighbors ()
    let get_velocity (dir, coord) =
      match get coord with
        | Some c when filter c ->
          Coord.border dir c.velocity
        | _ ->
          Vector3d.ZERO
    Seq.map get_velocity neighbors |> Seq.toList

  let divergence (where: Coord) =
    let vs = get_velocities (fun c -> c.is_not_solid ()) where
    let result = (vs.[0] .- vs.[1]) .+
                 (vs.[2] .- vs.[3]) .+
                 (vs.[4] .- vs.[5])
    result.x + result.y + result.z

  let number_neighbors fn (where: Coord) =
    let neighbors = where.neighbors ()
    let result = Seq.filter (fun (_, n) -> match get n with | Some c -> fn c | None -> false) neighbors
    result |> Seq.length |> float

  let pressure_gradient (where: Coord) =
    let c = raw_get where
    let p = Option.get c.pressure
    let neighbors = where.backward_neighbors ()
    let get_gradient (_, n) =
      match get n with
        | Some c when c.is_not_solid () ->
          Option.get c.pressure
        | _ ->
          0.0<kg/(m*s^2)>
    let n1 = get_gradient neighbors.[0]
    let n2 = get_gradient neighbors.[1]
    let n3 = get_gradient neighbors.[2]
    Vector3d<kg/(m*s^2)>(p - n1, p - n2, p - n3)

  // set the pressure such that the divergence throughout the fluid is zero.
  let apply markers dt =
    let n = Seq.length markers
    let lookups = Seq.zip markers { 0..n } |> dict
    let coefficients (coord: Coord) =
      let neighbors = coord.neighbors ()
      // return a 1 for every bordering liquid marker.
      let singulars = Seq.fold (fun accum (_, n) ->
                                  if lookups.ContainsKey n then
                                    (lookups.[n], 1.0) :: accum
                                  else
                                    accum
                                ) [] neighbors
      // return -N for this marker, where N is number of non solid neighbors.
      let N = number_neighbors (fun c -> c.is_not_solid ()) coord
      (lookups.[coord], - N) :: singulars
    // construct a sparse matrix of coefficients.
    let mutable m = SparseMatrix.zero<float> n n
    for KeyValue(marker, c) in lookups do
      for (r, value) in coefficients marker do
        m.[r, c] <- value
    if not (m.IsSymmetric ()) then
      failwith "Coefficient matrix is not symmetric."
    // calculate divergences of the velocity field.
    let c = (Constants.h * Constants.fluid_density) / dt
    let b = Seq.map (fun m ->
                       let f = divergence m
                       let a = number_neighbors (fun c -> c.media = Air) m
                       let s = Constants.atmospheric_pressure
                       let result: float<kg/(m*s^2)> = c * f - (s * a) // ensure we have units of pressure.
                       float result
                     ) markers
                     |> Seq.toList |> vector
    let pressures = m.Solve(b)
    // set pressures.
    for (m, p) in Seq.zip markers pressures do
      if Double.IsNaN (p / LanguagePrimitives.FloatWithMeasure 1.0) then
        failwith "Invalid pressure."
      if p < 0.0 then
        failwith "Pressure is negative."
      let c = Grid.raw_get m
      Grid.set m { c with pressure = Some (p * 1.0<kg/(m*s^2)>) }
    // calculate the resulting pressure gradient and set velocities.
    let inv_c = dt / Constants.h
    Seq.map (fun (m: Coord) ->
               // only adjust velocity components that border fluid cells
               let neighbors = m.neighbors ()
               let borders = Seq.filter (fun (dir, n) ->
                                           let d = Coord.reverse dir
                                           match get n with
                                             | Some c when c.is_not_solid () && Coord.is_bordering d c.velocity -> true
                                             | _ -> false
                                        ) neighbors
               let nvs = Seq.map (fun (dir, n) ->
                                    let d = Coord.reverse dir
                                    let c = Grid.raw_get n
                                    let new_velocity = Coord.border d c.velocity
                                    let gradient: Vector3d<kg/(m*s^2)> = pressure_gradient n
                                    let density = match c.media with
                                                    | Air -> Constants.air_density
                                                    | Fluid -> Constants.fluid_density
                                                    | _ -> failwith "no density for media found."
                                    let offset = gradient .* (inv_c / density)
                                    let v = new_velocity .- offset
                                    (n, v)) borders
               let c = Grid.raw_get m
               let gradient = pressure_gradient m
               let density = Constants.fluid_density
               let offset = gradient .* (inv_c / density)
               let v = c.velocity .- offset
               (m, v) :: (nvs |> Seq.toList)
            ) markers |> Seq.concat

  // verify that for each marker, ∇⋅u = 0.
  let check_divergence markers =
    for m in markers do
      let f = divergence m
      if f > 0.0001<m/s> || f < -0.0001<m/s> then
        failwith <| sprintf "Velocity field is not correct: %A has divergence %A." m f
