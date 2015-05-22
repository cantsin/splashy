module splashy.Tests

open splashy
open NUnit.Framework
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Coord
open Vector
open Cell

let air = { media = Air; pressure = None; velocity = Vector3d.ZERO }

[<Test>]
let ``divergence of standalone cell`` () =
  // setup.
  let coord = { x = 0<m>; y = 0<m>; z = 0<m>; }
  let c = (coord, { air with velocity = Vector3d(1.0<m/s>, 1.0<m/s>, 1.0<m/s>) })
  let n = Seq.map (fun (_, n) -> (n, air)) (coord.neighbors ()) |> Seq.toList
  Grid.add_cells (c :: n)
  // func.
  let d = Pressure.divergence coord
  Assert.AreEqual(d, -3.0<m/s>)
