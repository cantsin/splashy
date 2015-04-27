namespace splashy

open System.Collections.Generic

open Vector

module Grid =
  type Coord = { x: int; y: int; z: int }
  type Media = Air | Fluid | Solid
  type Cell = { pressure: float;
                media: Media;
                velocity: Vector3d; // from the minimal faces, not the center
                layer: int; }

  let default_cell = { pressure = 0.0; media = Air; layer = -1; velocity = Vector3d() }

  let to_vector where = Vector3d(float where.x, float where.y, float where.z)

  // configuration options.
  [<Literal>]
  let h = 0.1
  [<Literal>]
  let time_step_constant = 2.5

  let mutable grid = new Dictionary<int, Cell>()
  let mutable markers = new List<Coord>()

  let hash coords = 541 * coords.x + 79 * coords.y + 31 * coords.z

  let add where c = grid.Add (hash where, c)

  let delete where = grid.Remove (hash where)

  let get where =
    let key = hash where
    match grid.ContainsKey key with
      | true -> Some grid.[key]
      | _ -> None

  let set where c =
    match get where with
      | None -> failwith "Tried to set non-existent cell."
      | _ ->
        let key = hash where
        grid.[key] <- c

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
    let new_grid = Seq.map (fun (KeyValue(k, v)) -> (k, { v with layer = -1 })) grid
    grid.Clear()
    Seq.iter grid.Add new_grid
