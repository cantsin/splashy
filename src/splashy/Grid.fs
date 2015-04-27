namespace splashy

open System.Collections.Generic

open Vector

module Grid =

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

  type Media = Air | Fluid | Solid

  type Cell = { pressure: float;
                media: Media;
                velocity: Vector3d; // from the minimal faces, not the center
                layer: Option<int>; }

  let default_cell = { pressure = 0.0; media = Air; layer = None; velocity = Vector3d() }

  let to_vector where = Vector3d(float where.x, float where.y, float where.z)

  // configuration options.
  [<Literal>]
  let h = 0.1
  [<Literal>]
  let time_step_constant = 2.5

  let mutable grid = new Dictionary<Coord, Cell>()
  let mutable markers: Coord list = []

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
    Seq.filter (fun (KeyValue(k, v)) -> fn v) grid |> Seq.map (fun (KeyValue(k, v)) -> k)

  let neighbors c =
    [| { c with x = c.x + 1 };
       { c with x = c.x - 1 };
       { c with y = c.y + 1 };
       { c with y = c.y - 1 };
       { c with z = c.z + 1 };
       { c with z = c.z - 1 }; |]

  let is_solid c = match c.media with Solid -> true | _ -> false

  let reset () =
    let new_grid = Seq.map (fun (KeyValue(k, v)) -> (k, { v with layer = None })) grid
    grid.Clear()
    Seq.iter grid.Add new_grid
