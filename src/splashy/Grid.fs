namespace splashy

open System.Collections.Generic

open Vector

module Grid =

  // configuration options.
  [<Literal>]
  let h = 10.0
  [<Literal>]
  let time_step_constant = 2.5

  [<CustomEquality;CustomComparison>]
  type Coord =
    { x: int; y: int; z: int }
    override this.GetHashCode () =
      541 * this.x + 79 * this.y + 31 * this.z
    override this.Equals that =
      match that with
        | :? Coord as c -> this.x = c.x && this.y = c.y && this.z = c.z
        | _ -> false
    interface System.IComparable with
      member this.CompareTo that =
        match that with
          | :? Coord as c -> compare this c
          | _ -> invalidArg "Coord" "cannot compare values of different types."
    member this.to_vector () =
      Vector3d(float this.x, float this.y, float this.z)
    member this.neighbors () =
      let h = int h
      [| { this with x = this.x + h };
         { this with x = this.x - h };
         { this with y = this.y + h };
         { this with y = this.y - h };
         { this with z = this.z + h };
         { this with z = this.z - h }; |]

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

  let internal get_velocity_index where index =
    match grid.ContainsKey where with
      | true ->
        match index with
          | 0 -> Some grid.[where].velocity.x
          | 1 -> Some grid.[where].velocity.y
          | 2 -> Some grid.[where].velocity.z
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
                  | None -> yield 0.0]
    Seq.sum sums

  let internal get_velocity x y z =
    let xh = float x / h
    let yh = float y / h
    let zh = float z / h
    let x = interpolate xh (yh - 0.5) (zh - 0.5) 0
    let y = interpolate (xh - 0.5) yh (zh - 0.5) 1
    let z = interpolate (xh - 0.5) (yh - 0.5) zh 2
    Vector3d(x, y, z)

  let trace (c: Coord) t =
    // runge kutta order two interpolation
    let cv = c.to_vector ()
    let v = get_velocity cv.x cv.y cv.z
    let x = cv.x + 0.5 * t * v.x
    let y = cv.y + 0.5 * t * v.y
    let z = cv.z + 0.5 * t * v.z
    let dv = get_velocity x y z
    let p = cv .+ (dv .* t)
    { x = int p.x; y = int p.y; z = int p.z }
