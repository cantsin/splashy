module Splashy.Tests

open Splashy
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Constants
open Coord
open Vector
open Cell

// encapsulates the state of a fluid cell and its neighbors
type FluidData = { cell_velocity: Vector3d<m/s>;
                   mediums: Media list;
                   velocities: Vector3d<m/s> list }

type Generators =

  static member arb_velocity =
    Arb.generate<float<m/s>>
    |> Gen.suchThat Util.is_valid_unit
    |> Gen.three
    |> Gen.map (fun (x, y, z) -> Vector3d<m/s>(x, y, z))
    |> Arb.fromGen

  static member arb_neighbordata =
    Gen.map3
      (fun cv ms vs -> { cell_velocity = cv; mediums = ms; velocities = vs; })
      (Arb.generate<Vector3d<m/s>>)
      (Gen.listOfLength 6 Arb.generate<Media>)
      (Gen.listOfLength 6 Arb.generate<Vector3d<m/s>>)
    |> Arb.fromGen

[<TestFixture>]
type pressure_fixture () =

  let air = { media = Air; pressure = Some Constants.atmospheric_pressure; velocity = Vector3d.ZERO }
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
    Grid.filter (fun _ -> true) |> Grid.delete_cells

  [<Test>]
  member test.``divergence, no velocities`` () =
    let d = Pressure.divergence coord
    Assert.AreEqual(d, 0.0<m/s>)

  [<Test>]
  member test.``divergence, standalone cell`` () =
    Check.QuickThrowOnFailure(
      fun (v: Vector3d<m/s>) ->
        Grid.update_velocities [(coord, v)]
        Pressure.divergence coord = (- v.x - v.y - v.z))

  [<Test>]
  member test.``divergence, surrounded by rock`` () =
    let mediums = Seq.map (fun (_, n) -> (n, Solid)) neighbors |> Seq.toList
    Grid.update_media mediums
    Check.QuickThrowOnFailure(
      fun (v: Vector3d<m/s>) ->
        Grid.update_velocities [(coord, v)]
        Pressure.divergence coord = 0.0<m/s>)

  [<Test>]
  member test.``divergence, surrounded by random elements`` () =
    let (neighbor_dirs, neighbor_coords) = List.unzip (neighbors |> Seq.toList)
    let neighbor_index dir = List.findIndex ((=) dir) neighbor_dirs
    let get_divergence (center: Vector3d<m/s>) (mediums: Media list) (velocities: Vector3d<m/s> list) =
      let x1 = if mediums.[neighbor_index NegX] = Solid then 0.0<m/s> else center.x
      let y1 = if mediums.[neighbor_index NegY] = Solid then 0.0<m/s> else center.y
      let z1 = if mediums.[neighbor_index NegZ] = Solid then 0.0<m/s> else center.z
      let x2 = if mediums.[neighbor_index PosX] = Solid then 0.0<m/s> else velocities.[neighbor_index PosX].x
      let y2 = if mediums.[neighbor_index PosY] = Solid then 0.0<m/s> else velocities.[neighbor_index PosY].y
      let z2 = if mediums.[neighbor_index PosZ] = Solid then 0.0<m/s> else velocities.[neighbor_index PosZ].z
      // order of operations matters.
      let result = Vector3d(x2, y2, z2) .- Vector3d(x1, y1, z1)
      result.x + result.y + result.z
    Check.QuickThrowOnFailure(
      (fun (nd: FluidData) ->
         Grid.update_velocities [(coord, nd.cell_velocity)]
         Grid.update_velocities <| Seq.zip neighbor_coords nd.velocities
         Grid.update_media <| Seq.zip neighbor_coords nd.mediums
         let d = get_divergence nd.cell_velocity nd.mediums nd.velocities
         let real_d = Pressure.divergence coord
         (d = real_d) || (abs (real_d - d)) < 0.0000001<m/s>))

  [<Test>]
  member test.``pressure, no velocities`` () =
    let p = Pressure.gradient coord
    Assert.AreEqual(p, Vector3d<kg/(m*s^2)>.ZERO)

  [<Test>]
  member test.``pressure, surrounded by rock`` () =
    let mediums = Seq.map (fun (_, n) -> (n, Solid)) neighbors |> Seq.toList
    Grid.update_media mediums
    Check.QuickThrowOnFailure(
      fun (v: float<kg/(m*s^2)>) ->
        Grid.update_pressures [(coord, v)]
        (not <| Util.is_valid_unit v) || Pressure.gradient coord = Vector3d<kg/(m*s^2)>(v, v, v))
