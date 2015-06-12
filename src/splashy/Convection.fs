namespace Splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Vector
open Coord
open Grid

module Convection =

  let internal get_velocity_index where index =
    match get where with
      | Some c ->
        match index with
          | 0 -> Some c.velocity.x
          | 1 -> Some c.velocity.y
          | 2 -> Some c.velocity.z
          | _ -> failwith "No such velocity index."
      | _ -> None

  let internal interpolate x y z index =
    let i = floor x
    let j = floor y
    let k = floor z
    let ii = int i
    let jj = int j
    let kk = int k
    let id = [i - x + 1.0; x - i]
    let jd = [j - y + 1.0; y - j]
    let kd = [k - z + 1.0; z - k]
    // trilinear interpolation.
    let sums = [for x' in 0..1 do
                for y' in 0..1 do
                for z' in 0..1 do
                let c = Coord.construct(ii + x', jj + y', kk + z')
                match get_velocity_index c index with
                  | Some v -> yield (v * id.[x'] * jd.[y'] * kd.[z'])
                  | None -> yield 0.0<m/s>]
    Seq.sum sums

  let internal get_interpolated_velocity x y z =
    let xh = x / Constants.h
    let yh = y / Constants.h
    let zh = z / Constants.h
    let x = interpolate xh (yh - 0.5) (zh - 0.5) 0
    let y = interpolate (xh - 0.5) yh (zh - 0.5) 1
    let z = interpolate (xh - 0.5) (yh - 0.5) zh 2
    Vector3d<m/s>(x, y, z)

  let trace (c: Coord) (t: float<s>) =
    // runge kutta order two interpolation.
    let cv = Vector3d<m>(c.x, c.y, c.z)
    let v = get_interpolated_velocity cv.x cv.y cv.z
    let nv = cv .+ (v .* (t / 2.0))
    let dv = get_interpolated_velocity nv.x nv.y nv.z
    let p = cv .+ (dv .* t)
    Coord.construct(p.x, p.y, p.z)

  // convection by means of a backwards particle trace.
  let apply markers dt =
    Seq.map (fun m ->
               let new_position = trace m dt
               match Grid.get new_position with
                 | Some c -> (m, c.velocity)
                 | _ -> failwith "Backwards particle trace went too far."
            ) markers
