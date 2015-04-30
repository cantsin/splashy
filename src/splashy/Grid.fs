namespace splashy

open System.Collections.Generic

open Vector
open Coord

module Grid =

  type Media = Air | Fluid | Solid

  type Cell = { pressure: float;
                media: Media;
                velocity: Vector3d; // from the minimal faces, not the center
                layer: Option<int>; }

  let default_cell = { pressure = 0.0; media = Air; layer = None; velocity = Vector3d() }

  let mutable grid = new Dictionary<Coord, Cell>()

  let add where c = grid.Add (where, c)

  let delete where = let _ = grid.Remove where in ()

  let get where =
    match grid.ContainsKey where with
      | true -> Some grid.[where]
      | _ -> None

  let set where c =
    match get where with
      | None -> failwith "Tried to set non-existent cell."
      | _ ->
        grid.[where] <- c

  let filter_values fn =
    let result = Seq.filter (fun (KeyValue(k, v)) -> fn v) grid
    let keys = Seq.map (fun (KeyValue(k, v)) -> k) result
    // make a copy; we want to avoid writing to the dictionary while
    // potentially iterating over it.
    new List<Coord> (keys)

  let is_solid c = match c.media with Solid -> true | _ -> false

  let setup fn =
    try
      // reset grid layers.
      let coords = filter_values (fun _ -> true)
      Seq.iter (fun m ->
                match get m with
                  | Some c -> set m { c with layer = None }
                  | _ -> failwith "Could not get/set grid cell."
                ) coords
      fn ()
    finally
      // get rid of unused cells.
      let leftover = filter_values (fun c -> c.layer = None)
      Seq.iter delete leftover

  let internal get_velocity_index where index =
    match grid.ContainsKey where with
      | true ->
        let c = grid.[where]
        if c.media <> Solid then
          match index with
            | 0 -> Some c.velocity.x
            | 1 -> Some c.velocity.y
            | 2 -> Some c.velocity.z
            | _ -> failwith "No such index."
        else
          None
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
                  | None -> yield 0.0]
    Seq.sum sums

  let internal get_interpolated_velocity x y z =
    let xh = float x / Coord.h
    let yh = float y / Coord.h
    let zh = float z / Coord.h
    let x = interpolate xh (yh - 0.5) (zh - 0.5) 0
    let y = interpolate (xh - 0.5) yh (zh - 0.5) 1
    let z = interpolate (xh - 0.5) (yh - 0.5) zh 2
    Vector3d(x, y, z)

  let trace (c: Coord) t =
    // runge kutta order two interpolation
    let cv = c.to_vector ()
    let v = get_interpolated_velocity cv.x cv.y cv.z
    let x = cv.x + 0.5 * t * v.x
    let y = cv.y + 0.5 * t * v.y
    let z = cv.z + 0.5 * t * v.z
    let dv = get_interpolated_velocity x y z
    let p = cv .+ (dv .* t)
    let to_int x = round x |> int
    { x = to_int p.x; y = to_int p.y; z = to_int p.z }

  let internal get_shared_velocity d n =
    match get n with
      | Some c when c.media = Fluid && is_bordering d c.velocity ->
        c.velocity
      | _ ->
        Vector3d()

  let laplacian (where: Coord) =
    let neighbors = where.neighbors ()
    let where_v = match get where with
                    | Some c -> c.velocity .* 6.0
                    | None -> Vector3d()
    let v = Seq.fold (fun accum (d, n) -> accum .+ get_shared_velocity d n) (Vector3d()) neighbors
    v .- where_v
