namespace Splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

open Util

module Vector =
  type Vector3d<[<Measure>] 'u> =
    struct
      val x: float<'u>
      val y: float<'u>
      val z: float<'u>

      new(x: float<'u>, y: float<'u>, z:float<'u>) =
        if not (Util.is_valid_unit x && Util.is_valid_unit y && Util.is_valid_unit z) then
          failwith <| sprintf "Vector3d [%A; %A; %A] has an invalid quantity: either NaN or ±Infinity." x y z
        { x = x; y = y; z = z; }

      new(x: int<'u>, y: int<'u>, z:int<'u>) =
        { x = float x * LanguagePrimitives.FloatWithMeasure 1.0;
          y = float y * LanguagePrimitives.FloatWithMeasure 1.0;
          z = float z * LanguagePrimitives.FloatWithMeasure 1.0; }

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
