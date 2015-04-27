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
    // make a copy; we want to avoid writing to the dictionary while iterating over it.
    new List<Coord> (keys)

  let is_solid c = match c.media with Solid -> true | _ -> false

  let reset () =
    let keys = filter_values (fun _ -> true)
    Seq.iter (fun k -> set k { grid.[k] with layer = None }) keys
