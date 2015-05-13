namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open System

module Vector =
  type Vector3d<[<Measure>] 'u> =
    struct
      val x: float<'u>
      val y: float<'u>
      val z: float<'u>

      new(x: float<'u>, y: float<'u>, z:float<'u>) =
        let is_nan f = Double.IsNaN (f / LanguagePrimitives.FloatWithMeasure 1.0)
        if is_nan x || is_nan y || is_nan z then
          failwith "Vector3d got NaN."
        { x = x; y = y; z = z; }

      // set up explicit operators, as I dislike overloading common operators.
      static member (.+) (v1: Vector3d<'u>, v2: Vector3d<'u>) = Vector3d<'u>(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)

      static member (.-) (v1: Vector3d<'u>, v2: Vector3d<'u>) = Vector3d<'u>(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)

      static member (.*) (v: Vector3d<'u>, f: float<'v>) = Vector3d<_>(v.x * f, v.y * f, v.z * f)

      static member (./) (v: Vector3d<'u>, f: float<'v>) = Vector3d<_>(v.x / f, v.y / f, v.z / f)

      static member ZERO = Vector3d<'u>()

      override this.ToString () = sprintf "Vector3d[%A; %A; %A]" this.x this.y this.z

    end

  let sum v = Seq.fold (.+) Vector3d<_>.ZERO v

  let average v = (sum v) ./ float (Seq.length v)
