namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System
open OpenTK

open Constants
open Cell
open Vector
open Aabb
open Coord
open Grid
open Layer
open World
open Convection
open Viscosity
open Pressure
open Forces

module Simulator =


  let mutable markers: Coord list = []         // closest coordinate locations.
  let mutable locations: Vector3d<m> list = [] // real locations.

  // synchronize our fluid markers with the grid.
  let sync_markers () =
    for marker in markers do
      match Grid.get marker with
        | Some c when c.is_not_solid () ->
          Grid.set marker { c with media = Fluid; layer = Some 0; }
        | None ->
          if Aabb.contains World.world marker then
            Grid.add marker { Cell.default_cell with media = Fluid; layer = Some 0; }
        | _ ->
          ()

  // for now, advance by frame.
  let move_markers dt =
    locations <- Seq.map (fun (l: Vector3d<m>) ->
                            let m = Coord.construct(l.x, l.y, l.z)
                            let c = Grid.raw_get m
                            l .+ (c.velocity .* dt)
                          ) locations |> Seq.toList
    markers <- Seq.map (fun (l: Vector3d<m>) -> Coord.construct(l.x, l.y, l.z)) locations |> Seq.toList

  let advance dt =
    let dt = 0.0066
    let dt = dt * 1.0<s> // * Constants.time_step
    printfn "-->"
    printfn "Moving simulation forward with time step %A." dt
    Layer.setup (fun () ->
      printfn "  Setup: Synchronizing fluid markers."
      sync_markers ()
      printfn "  Setup: Creating air buffer."
      Layer.create_air_buffer ()
    )
    // sanity check, part 1.
    printfn "* Verifying divergence (1)."
    Pressure.check_divergence markers
    printfn "Applying convection."
    Convection.apply_convection markers dt // -(∇⋅u)u
    printfn "Applying forces."
    Forces.apply_forces markers dt         // F
    printfn "Applying viscosity."
    Viscosity.apply_viscosity markers dt   // v∇²u
    printfn "Applying pressure."
    Pressure.apply_pressure markers dt     // -1/ρ∇p
    // sanity check, part 2.
    printfn "* Verifying divergence (2)."
    Pressure.check_divergence markers
    printfn "Cleaning up grid."
    Layer.cleanup (fun () ->
      printfn "  Cleanup: Propagating fluid velocities into surroundings."
      Layer.propagate_velocities ()
      printfn "  Cleanup: Setting solid cell velocities to zero."
      Layer.zero_solid_velocities()
    )
    printfn "Moving fluid markers."
    move_markers dt
    // sanity check, part 3.
    printfn "* Verifying containment."
    Seq.iter (fun (m: Coord) ->
                if not (Aabb.contains World.world m) then
                  failwith (sprintf "Error: Fluid markers went outside bounds (example: %O)." m)
             ) markers

  // generate a random amount of markers to begin with (testing purposes only).
  let generate n =
    let seed = 12345
    let r = System.Random(seed)
    let h = int Constants.h
    let l = int (Constants.world_h / float32 Constants.h)
    let new_markers = [ for _ in 0..n-1 do
                        let x = r.Next(-l, l + 1) * h
                        let y = r.Next(-l, l + 1) * h
                        let z = r.Next(-l, l + 1) * h
                        yield Coord.construct(x, y, z) ]
    markers <- Set.ofList new_markers |> Seq.toList
    let to_vec m = Vector3d<m>(float m.x * 1.0<m>, float m.y * 1.0<m>, float m.z * 1.0<m>)
    locations <- Seq.map to_vec markers |> Seq.toList
    sync_markers ()
    let actual = Seq.length markers
    if actual <> n then
      printfn "Warning: could only generate %d random markers." actual
