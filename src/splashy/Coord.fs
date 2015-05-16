namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open OpenTK

open Constants
open Vector

module Coord =

  type CoordDirection = NegX | NegY | NegZ | PosX | PosY | PosZ

  [<CustomEquality>]
  [<CustomComparison>]
  type Coord =
    { x: int<m>; y: int<m>; z: int<m>; }

    override this.GetHashCode () =
      541 * int this.x + 79 * int this.y + 31 * int this.z

    override this.Equals that =
      match that with
        | :? Coord as c -> this.x = c.x && this.y = c.y && this.z = c.z
        | _ -> false

    static member compare v1 v2 =
      let cmp x y = if x = y then 0 else if x > y then 1 else -1
      let x1 = cmp v1.x v2.x
      let x2 = cmp v1.y v2.y
      let x3 = cmp v1.z v2.z
      match x1 with
        | 0 ->
          match x2 with
            | 0 -> x3
            | n -> n
        | n -> n

    interface System.IComparable with
      member this.CompareTo that =
        match that with
          | :? Coord as c -> Coord.compare this c
          | _ -> invalidArg "Coord" "cannot compare values of different types."

    static member nearest n =
      let h = int Constants.h
      let r = n % h
      if r = 0 then
        n
      else if r >= (h / 2) then
        n + (h - r)
      else
        n - r

    static member construct(x: int, y: int, z:int) =
      let to_nearest x = Coord.nearest x * 1<m>
      { x = to_nearest x; y = to_nearest y; z = to_nearest z; }

    // NB. round only works on dimensionless floats, so do a raw
    // conversion from float<m> to int<m>.
    static member construct(x: float<m>, y: float<m>, z:float<m>) =
      let to_nearest_int x = float x |> round |> int |> Coord.nearest |> fun n -> n * 1<m>
      { x = to_nearest_int x; y = to_nearest_int y; z = to_nearest_int z; }

    member this.neighbors () =
      let h = int Constants.h * 1<m>
      [| PosX, { this with x = this.x + h };
         NegX, { this with x = this.x - h };
         PosY, { this with y = this.y + h };
         NegY, { this with y = this.y - h };
         PosZ, { this with z = this.z + h };
         NegZ, { this with z = this.z - h }; |]

    member this.backward_neighbors () =
      let h = int Constants.h * 1<m>
      [| NegX, { this with x = this.x - h };
         NegY, { this with y = this.y - h };
         NegZ, { this with z = this.z - h }; |]

    member this.forward_neighbors () =
      let h = int Constants.h * 1<m>
      [| PosX, { this with x = this.x + h };
         PosY, { this with y = this.y + h };
         PosZ, { this with z = this.z + h }; |]

    member this.get_neighbor dir =
      let h = int Constants.h * 1<m>
      match dir with
        | NegX -> { this with x = this.x - h }
        | PosX -> { this with x = this.x + h }
        | NegY -> { this with y = this.y - h }
        | PosY -> { this with y = this.y + h }
        | NegZ -> { this with z = this.z - h }
        | PosZ -> { this with z = this.z + h }

    override this.ToString () = sprintf "Coord[%A; %A; %A]" this.x this.y this.z

  let get_borders (v: Vector3d<'u>) =
    let mutable (borders: CoordDirection list) = []
    if v.x < 0.0<_> then borders <- NegX :: borders
    if v.x > 0.0<_> then borders <- PosX :: borders
    if v.y < 0.0<_> then borders <- NegY :: borders
    if v.y > 0.0<_> then borders <- PosY :: borders
    if v.z < 0.0<_> then borders <- NegZ :: borders
    if v.z > 0.0<_> then borders <- PosZ :: borders
    borders

  let reverse d =
    match d with
      | NegX -> PosX
      | PosX -> NegX
      | NegY -> PosY
      | PosY -> NegY
      | NegZ -> PosZ
      | PosZ -> NegZ

  let is_bordering d (v: Vector3d<'u>) =
    match d with
      | NegX when v.x < 0.0<_> -> true
      | PosX when v.x > 0.0<_> -> true
      | NegY when v.y < 0.0<_> -> true
      | PosY when v.y > 0.0<_> -> true
      | NegZ when v.z < 0.0<_> -> true
      | PosZ when v.z > 0.0<_> -> true
      | _ -> false

  let border d (v: Vector3d<'u>) =
    match d with
      | NegX | PosX -> Vector3d<'u>(v.x, 0.0<_>, 0.0<_>)
      | NegY | PosY -> Vector3d<'u>(0.0<_>, v.y, 0.0<_>)
      | NegZ | PosZ -> Vector3d<'u>(0.0<_>, 0.0<_>, v.z)

  let border_value d (v: Vector3d<'u>) =
    match d with
      | NegX | PosX -> v.x
      | NegY | PosY -> v.y
      | NegZ | PosZ -> v.z

  let merge d (old_v: Vector3d<'u>) (new_v: Vector3d<'u>) =
    match d with
      | NegX | PosX -> Vector3d<'u>(new_v.x, old_v.y, old_v.z)
      | NegY | PosY -> Vector3d<'u>(old_v.x, new_v.y, old_v.z)
      | NegZ | PosZ -> Vector3d<'u>(old_v.x, old_v.y, new_v.z)
