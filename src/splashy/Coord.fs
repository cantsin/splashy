namespace splashy

open Vector

module Coord =
  // configuration options.
  [<Literal>]
  let h = 10.0

  type CoordDirection = NegX | NegY | NegZ | PosX | PosY | PosZ

  [<CustomEquality;CustomComparison>]
  type Coord =
    { x: int; y: int; z: int }
    override this.GetHashCode () =
      541 * this.x + 79 * this.y + 31 * this.z
    override this.Equals that =
      match that with
        | :? Coord as c -> this.x = c.x && this.y = c.y && this.z = c.z
        | _ -> false
    interface System.IComparable with
      member this.CompareTo that =
        match that with
          | :? Coord as c -> compare this c
          | _ -> invalidArg "Coord" "cannot compare values of different types."
    member this.to_vector () =
      Vector3d(float this.x, float this.y, float this.z)
    member this.neighbors () =
      let h = int h
      [| PosX, { this with x = this.x + h };
         NegX, { this with x = this.x - h };
         PosY, { this with y = this.y + h };
         NegY, { this with y = this.y - h };
         PosZ, { this with z = this.z + h };
         NegZ, { this with z = this.z - h }; |]

  let internal is_bordering d (v: Vector3d) =
    match d with
      | NegX when v.x < 0.0 -> true
      | PosX when v.x > 0.0 -> true
      | NegY when v.y < 0.0 -> true
      | PosY when v.y > 0.0 -> true
      | NegZ when v.z < 0.0 -> true
      | PosZ when v.z > 0.0 -> true
      | _ -> false
