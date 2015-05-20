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
open Convection
open Viscosity
open Pressure
open Forces

module Simulator =

  let mutable private markers: Coord list = []         // closest coordinate locations.
  let mutable private locations: Vector3d<m> list = [] // real locations.

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
      Layer.sync_markers markers
      printfn "  Setup: Creating air buffer."
      Layer.create_air_buffer ()
      printfn "  Setup: Removing unused layers."
      Layer.delete_unused ()
    )
    // sanity check, part 1.
    printfn "* Verifying divergence (1)."
    Pressure.check_divergence markers
    printfn "Applying convection term -(∇⋅u)u."
    Convection.apply markers dt |> Layer.update_velocities
    printfn "Applying external forces term F."
    Forces.apply markers dt |> Layer.update_velocities
    printfn "Applying viscosity term v∇²u."
    Viscosity.apply markers dt |> Layer.update_velocities
    printfn "Applying pressure term -1/ρ∇p."
    Pressure.calculate markers dt |> Layer.update_pressures
    Pressure.apply markers dt |> Layer.update_velocities
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
    Layer.check_containment markers

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
    let actual = Seq.length markers
    if actual <> n then
      printfn "Warning: could only generate %d random markers." actual
