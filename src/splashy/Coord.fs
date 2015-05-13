namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open OpenTK

open Constants
open Vector

module Coord =

  type CoordDirection = NegX | NegY | NegZ | PosX | PosY | PosZ

  //[<CustomEquality;CustomComparison>]
  type Coord =
    { x: int<m>; y: int<m>; z: int<m> }

    // override this.GetHashCode () =
    //   541 * this.x + 79 * this.y + 31 * this.z

    // override this.Equals that =
    //   match that with
    //     | :? Coord as c -> this.x = c.x && this.y = c.y && this.z = c.z
    //     | _ -> false

    // interface System.IComparable with
    //   member this.CompareTo that =
    //     match that with
    //       | :? Coord as c -> compare this c
    //       | _ -> invalidArg "Coord" "cannot compare values of different types."

    static member construct(x: int, y: int, z:int) =
      { x = x * 1<m>; y = y * 1<m>; z = z * 1<m>; }

    // NB. round only works on dimensionless floats, so do a raw conversion from float<m> to int<m>
    static member construct(x: float<m>, y: float<m>, z:float<m>) =
      let to_int x = float x |> round |> int
      { x = to_int x * 1<m>; y = to_int y * 1<m>; z = to_int z * 1<m>; }

    member this.neighbors () =
      let h = int Constants.h * 1<m>
      [| PosX, { this with x = this.x + h };
         NegX, { this with x = this.x - h };
         PosY, { this with y = this.y + h };
         NegY, { this with y = this.y - h };
         PosZ, { this with z = this.z + h };
         NegZ, { this with z = this.z - h }; |]

    member this.backwardNeighbors () =
      let h = int Constants.h * 1<m>
      [| NegX, { this with x = this.x - h };
         NegY, { this with y = this.y - h };
         NegZ, { this with z = this.z - h }; |]

    member this.forwardNeighbors () =
      let h = int Constants.h * 1<m>
      [| PosX, { this with x = this.x + h };
         PosY, { this with y = this.y + h };
         PosZ, { this with z = this.z + h }; |]

    override this.ToString () = sprintf "Coord[%A; %A; %A]" this.x this.y this.z

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

  let merge d (old_v: Vector3d<'u>) (new_v: Vector3d<'u>) =
    match d with
      | NegX | PosX -> Vector3d<'u>(new_v.x, old_v.y, old_v.z)
      | NegY | PosY -> Vector3d<'u>(old_v.x, new_v.y, old_v.z)
      | NegZ | PosZ -> Vector3d<'u>(old_v.x, old_v.y, new_v.z)
