namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Cell
open Vector
open Coord
open Grid

module Viscosity =

  let internal get_shared_velocity (d, n) =
    match get n with
      | Some c when c.media = Fluid && Coord.is_bordering d c.velocity ->
        Coord.border d c.velocity
      | _ ->
        Vector3d.ZERO

  let laplacian (where: Coord) =
    let neighbors = where.neighbors ()
    let where_v = match get where with
                    | Some c -> c.velocity .* 6.0
                    | None -> Vector3d.ZERO
    let v = Seq.map get_shared_velocity neighbors |> Vector.sum
    let result = v .- where_v
    result .* 1.0<1/(m^2)>

  // viscosity by evaluating the lapalacian on bordering fluid cells.
  let apply_viscosity markers dt =
    let const_v: float<m^2> = Constants.fluid_viscosity * dt
    let velocities = Seq.map laplacian markers
    Seq.iter2 (fun m result ->
                 let c = Grid.raw_get m
                 let dv = result .* const_v
                 Grid.set m { c with velocity = c.velocity .+ dv }
               ) markers velocities
