namespace splashy

open System.Collections.Generic
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Constants
open Vector
open Coord

module Grid =

  type Media = Air | Fluid | Solid

  type Cell =
    { media: Media;
      velocity: Vector3d<m/s>; // from the minimal faces, not the center
      pressure: float<kg/(m*s^2)>;
      layer: Option<int>; }

    member this.is_solid () = match this.media with Solid -> true | _ -> false

    member this.is_not_solid () = not (this.is_solid ())

  let default_cell = { pressure = 0.0<kg/(m*s^2)>; media = Air; layer = None; velocity = Vector3d.ZERO }

  let max_distance = Operators.max 2 (int (ceil Constants.time_step_constant))

  let mutable grid = new Dictionary<Coord, Cell>()

  let add where c = grid.Add (where, c)

  let delete where = ignore <| grid.Remove where

  let get where =
    match grid.ContainsKey where with
      | true -> Some grid.[where]
      | _ -> None

  // unsafe. use only when absolutely certain that a corresponding grid cell exists.
  let raw_get where = get where |> Option.get

  let set where c =
    match get where with
      | None -> failwith "Tried to set non-existent cell."
      | _ -> grid.[where] <- c

  let filter_values fn =
    let result = Seq.filter (fun (KeyValue(k, v)) -> fn v) grid
    let keys = Seq.map (fun (KeyValue(k, v)) -> k) result
    // make a copy; we want to avoid writing to the dictionary while
    // potentially iterating over it.
    new List<Coord> (keys)

  let setup fn =
    try
      // reset grid layers.
      let coords = filter_values (fun _ -> true)
      Seq.iter (fun m ->
                  let c = raw_get m
                  set m { c with layer = None }
                ) coords
      fn ()
    finally
      // get rid of unused layers.
      let leftover = filter_values (fun c -> c.layer = None)
      Seq.iter delete leftover

  let cleanup fn =
    // reset grid layers but mark fluids as layer 0.
    let coords = filter_values (fun _ -> true)
    Seq.iter (fun m ->
                let c = raw_get m
                set m { c with layer = if c.media = Fluid then Some 0 else None }
              ) coords
    fn ()

  let internal get_velocity_index where index =
    match get where with
      | Some c ->
        match index with
          | 0 -> Some c.velocity.x
          | 1 -> Some c.velocity.y
          | 2 -> Some c.velocity.z
          | _ -> failwith "No such index."
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
    // trilinear interpolation
    let sums = [for x' in 0..1 do
                for y' in 0..1 do
                for z' in 0..1 do
                let c = { x = ii + x'; y = jj + y'; z = kk + z' }
                match get_velocity_index c index with
                  | Some v -> yield (v * id.[x'] * jd.[y'] * kd.[z'])
                  | None -> yield 0.0<m/s>]
    Seq.sum sums

  let internal get_interpolated_velocity x y z =
    let xh = float x / Constants.h
    let yh = float y / Constants.h
    let zh = float z / Constants.h
    let x = interpolate xh (yh - 0.5) (zh - 0.5) 0
    let y = interpolate (xh - 0.5) yh (zh - 0.5) 1
    let z = interpolate (xh - 0.5) (yh - 0.5) zh 2
    Vector3d(x, y, z)

  let trace (c: Coord) (t: float<s>) =
    // runge kutta order two interpolation
    let cv = Vector3d<m>(float c.x * 1.0<m>, float c.y * 1.0<m>, float c.z * 1.0<m>)
    let v = get_interpolated_velocity cv.x cv.y cv.z
    let x = cv.x + 0.5 * t * v.x
    let y = cv.y + 0.5 * t * v.y
    let z = cv.z + 0.5 * t * v.z
    let dv = get_interpolated_velocity x y z
    let p = cv .+ (dv .* t)
    let to_int x = round (x * 1.0<1/m>) |> int
    { x = to_int p.x; y = to_int p.y; z = to_int p.z }

  let internal get_shared_velocity d n =
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
    let vs = Seq.map (fun (d, n) -> get_shared_velocity d n) neighbors
    let v = Vector.sum vs
    let result = v .- where_v
    [result.x * 1.0<1/m^2>; result.y * 1.0<1/m^2>; result.z * 1.0<1/m^2>]

  // for the divergence, we want to ignore velocity components between
  // fluid and solid cells
  let internal get_shared_velocity' v d n =
    match get n with
      | Some c when c.is_not_solid () && Coord.is_bordering d c.velocity ->
        let nv = Coord.border d c.velocity
        let cv = Coord.border d v
        let result = nv .- cv
        result.x + result.y + result.z
      | _ ->
        0.0<m/s>

  let divergence (where: Coord) =
    let neighbors = where.forwardNeighbors ()
    let v = match get where with
              | Some c -> c.velocity
              | None -> Vector3d.ZERO
    Seq.map (fun (d, n) -> get_shared_velocity' v d n) neighbors |> Seq.sum

  let number_neighbors fn (where: Coord) =
    let neighbors = where.neighbors ()
    let result = Seq.filter (fun (_, n) -> match get n with | Some c -> fn c | None -> false) neighbors
    result |> Seq.length |> float
