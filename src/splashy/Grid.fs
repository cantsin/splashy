namespace splashy

open System.Collections.Generic

open Cell
open Coord

module Grid =

  let mutable private grid = new Dictionary<Coord, Cell>()

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

  let filter (fn: Cell -> bool) =
    // make a copy: we want to avoid writing to the dictionary while potentially iterating over it.
    Seq.filter (fun (KeyValue(k, v)) -> fn v) grid |>
    Seq.map (fun (KeyValue(k, v)) -> k) |>
    fun keys -> new List<Coord> (keys)
