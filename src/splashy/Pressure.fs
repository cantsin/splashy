namespace splashy

open MathNet.Numerics.LinearAlgebra
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

open Cell
open Vector
open Coord
open Grid

module Pressure =

  let divergence (where: Coord) =
    let cell = Grid.raw_get where
    let is_nonsolid (_, n) = let c = Grid.raw_get n in c.is_not_solid ()
    let backward = where.backward_neighbors ()
    let outgoing = Seq.filter is_nonsolid backward
                   |> Seq.fold (fun accum (dir, _) ->
                                  let v = Coord.border dir cell.velocity
                                  accum .+ v
                               ) Vector3d.ZERO
    let forward = where.forward_neighbors ()
    let incoming = Seq.filter is_nonsolid forward
                   |> Seq.fold (fun accum (dir, n) ->
                                  let c = raw_get n
                                  let v = Coord.border dir c.velocity
                                  accum .+ v
                               ) Vector3d.ZERO
    let result = incoming .- outgoing
    result.x + result.y + result.z

  let number_neighbors fn (where: Coord) =
    where.neighbors ()
    |> Seq.filter (fun (_, n) -> match get n with | Some c -> fn c | None -> false)
    |> Seq.length
    |> float

  // calculate the new pressures based on divergence of the velocity field.
  let calculate markers dt =
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
      let N = number_neighbors Cell.media_is_not_solid coord
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
                       float result // remove pressure units (matrix solver does not support units).
                     ) markers
                     |> Seq.toList
                     |> vector
    let pressures = m.Solve(b)
    // add the pressure units back.
    let pressure_units = Seq.map (fun p -> p * 1.0<kg/(m*s^2)>) pressures
    Seq.zip markers pressure_units

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
    let inv_c = dt / Constants.h
    Seq.map (fun (m: Coord) ->
               // only adjust velocity components that border fluid cells
               let gradient = pressure_gradient m
               let neighbors = m.forward_neighbors ()
               let borders = Seq.filter (fun (_, n) ->
                                           match get n with
                                             | Some c when c.is_not_solid () -> true
                                             | _ -> false
                                        ) neighbors
               let nvs = Seq.map (fun (dir, n) ->
                                    let c = Grid.raw_get n
                                    let density = match c.media with
                                                    | Air -> Constants.air_density
                                                    | Fluid -> Constants.fluid_density
                                                    | _ -> failwith "no density for media found."
                                    let gradient = pressure_gradient n
                                    let offset = gradient .* (inv_c / density)
                                    let v = Coord.border dir (c.velocity .- offset)
                                    (n, v)) borders
               let c = Grid.raw_get m
               let density = Constants.fluid_density
               let offset = gradient .* (inv_c / density)
               let v = c.velocity .- offset
               (m, v) :: (nvs |> Seq.toList)
            ) markers |> Seq.concat

  // verify that pressures look sane.
  let check_pressures markers =
    for m in markers do
      let c = Grid.raw_get m
      let p = c.pressure |> Option.get |> (fun x -> x / LanguagePrimitives.FloatWithMeasure 1.0)
      if Double.IsNaN p then
        failwith "Invalid pressure."
      if Double.IsNegativeInfinity p then
        failwith "Pressure is negative infinity."
      if Double.IsPositiveInfinity p then
        failwith "Pressure is positive infinity."
      if p < 0.0 then
        failwith "Pressure is negative."

  // verify that for each marker, ∇⋅u = 0.
  let check_divergence markers =
    for m in markers do
      let f = divergence m
      if f > 0.0001<m/s> || f < -0.0001<m/s> then
        failwith <| sprintf "Velocity field is not correct: %A has divergence %A." m f
