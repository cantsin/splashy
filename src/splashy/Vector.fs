namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open System

module Vector =
  type Vector3d =
    struct
      val x: float<m/s>
      val y: float<m/s>
      val z: float<m/s>

      new(x: float<m/s>, y: float<m/s>, z:float<m/s>) =
        let is_nan f = Double.IsNaN (f * 1.0<s/m>)
        if is_nan x || is_nan y || is_nan z then
          failwith "Vector3d got NaN."
        { x = x; y = y; z = z; }

      // set up explicit operators, as i dislike overloading common operators.
      static member (.+) (v1: Vector3d, v2: Vector3d) = Vector3d(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)

      static member (.-) (v1: Vector3d, v2: Vector3d) = Vector3d(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)

      static member (.*) (v: Vector3d, f) = Vector3d(v.x * f, v.y * f, v.z * f)

      static member (./) (v: Vector3d, f) = Vector3d(v.x / f, v.y / f, v.z / f)

      static member NONE = Vector3d()

      override this.ToString () = sprintf "Vector3d[%A; %A; %A]" this.x this.y this.z

    end

  let average v = (Seq.fold (.+) Vector3d.NONE v) ./ float (Seq.length v)
