module splashy.Tests

open splashy
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

open Coord
open Vector
open Cell

type Generators =
  static member arbVelocity =
    Arb.generate<float<m/s>>
    |> Gen.suchThat (fun f -> not (Double.IsNaN (f / LanguagePrimitives.FloatWithMeasure 1.0)))
    |> Gen.three
    |> Gen.map (fun (x, y, z) -> Vector3d<m/s>(x, y, z))
    |> Arb.fromGen

[<TestFixture>]
type pressure_fixture () =

  let air = { media = Air; pressure = None; velocity = Vector3d.ZERO }
  let coord = { x = 0<m>; y = 0<m>; z = 0<m>; }
  let neighbors = coord.neighbors ()

  [<SetUp>]
  member test.init () =
    Arb.register<Generators>() |> ignore
    let c = (coord, air)
    let n = Seq.map (fun (_, n) -> (n, air)) neighbors |> Seq.toList
    Grid.add_cells (c :: n)

  [<TearDown>]
  member test.destroy () =
    let rest = Seq.map (fun (_, n) -> n) neighbors |> Seq.toList
    Grid.delete_cells (coord :: rest)

  [<Test>]
  member test.``divergence, no velocities`` () =
    let d = Pressure.divergence coord
    Assert.AreEqual(d, 0.0<m/s>)

  [<Test>]
  member test.``divergence, standalone cell`` () =
    Check.QuickThrowOnFailure(
      fun (v: Vector3d<m/s>) ->
        Grid.update_velocities [(coord, v)]
        Pressure.divergence coord = - (v.x + v.y + v.z))

  [<Test>]
  member test.``divergence, surrounded by rock`` () =
    Grid.update_velocities [(coord, Vector3d(1.0<m/s>, 1.0<m/s>, 1.0<m/s>))]
    let mediums = Seq.map (fun (_, n) -> (n, Solid)) neighbors |> Seq.toList
    Grid.update_media mediums
    let d = Pressure.divergence coord
    Assert.AreEqual(d, 0.0<m/s>)

  [<Test>]
  member test.``divergence, surrounded by random rock`` () =
    Grid.update_velocities [(coord, Vector3d(1.0<m/s>, 1.0<m/s>, 1.0<m/s>))]
    let mediums = Seq.map (fun (_, n) -> (n, Solid)) neighbors |> Seq.toList
    Grid.update_media mediums
    let d = Pressure.divergence coord
    Assert.AreEqual(d, 0.0<m/s>)
