namespace splashy

open System.Collections.Generic

open Vector

module Grid =
  type Media = Air | Fluid | Solid
  type Coord = { x: int; y: int; z: int }
  type Cell = { pressure: float;
                media: Media;
                velocity: Vector3d; // from the minimal faces, not the center
                layer: int; }

  // configuration options.
  [<Literal>]
  let h = 1.0
  [<Literal>]
  let time_step_constant = 2.5

  let mutable grid = new Dictionary<int, Cell>()
  let mutable markers = new List<Coord>()

  let hash coords = 541 * coords.x + 79 * coords.y + 31 * coords.z

  let add where c = grid.Add (hash where, c)

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

  let reset () =
    let new_grid = Seq.map (fun (KeyValue(k, v)) -> (k, { v with layer = -1 })) grid
    grid.Clear()
    Seq.iter grid.Add new_grid
