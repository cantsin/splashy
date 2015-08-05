namespace Splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Collections.Generic

open Vector
open Cell
open Coord

module Grid =

  let mutable private grid = new Dictionary<Coord, Cell>()

  let get where =
    match grid.ContainsKey where with
      | true -> Some grid.[where]
      | _ -> None

  // unsafe. use only when absolutely certain that a corresponding grid cell exists.
  let raw_get where = get where |> Option.get

  let filter (fn: Cell -> bool) =
    // make a copy: we want to avoid writing to the dictionary while potentially iterating over it.
    grid
    |> Seq.filter (fun (KeyValue(k, v)) -> fn v)
    |> Seq.map (fun (KeyValue(k, v)) -> k)
    |> fun keys -> new List<Coord> (keys)

  let private delete where = ignore <| grid.Remove where

  let private add (where, c) = grid.Add (where, c)

  let private set where c =
    match get where with
      | None -> failwith "Tried to set non-existent cell."
      | _ -> grid.[where] <- c

  let add_cells (cells: seq<Coord * Cell>) = Seq.iter add cells

  let delete_cells (cells: seq<Coord>) = Seq.iter delete cells

  let move_cells (changes: seq<Coord * Coord>) =
    for (old_coord, new_coord) in changes do
      let c = raw_get old_coord
      delete new_coord
      add (new_coord, c)
      delete old_coord

  let update_velocities (velocities: seq<Coord * Vector3d<m/s>>) =
    Seq.iter (fun (where, new_v) ->
                let c = raw_get where
                set where { c with velocity = new_v }
              ) velocities

  let update_pressures (pressures: seq<Coord * float<kg/(m*s^2)>>) =
    Seq.iter (fun (where, new_p) ->
                let c = raw_get where
                set where { c with pressure = Some new_p }
              ) pressures

  // helper function to get surrounding velocities.
  let velocities_of (coord: Coord) =
    let backwards = coord.backward_neighbors ()
                   |> Seq.filter (fun (_, where) -> grid.ContainsKey where)
                   |> Seq.map (fun (dir, where) ->
                                 let v = (raw_get where).velocity
                                 let new_v = Coord.merge dir Vector3d.ZERO v
                                 (where, new_v, dir)
                              )
                   |> Seq.toList
    let v = (raw_get coord).velocity
    let forwards = [(coord, v.unit_x, PosX);
                    (coord, v.unit_y, PosY);
                    (coord, v.unit_z, PosZ);]
    forwards @ backwards
