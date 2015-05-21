namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Vector

module Cell =

  type Media = Air | Fluid | Solid

  type Cell =
    {
      media: Media;
      velocity: Vector3d<m/s>; // from the minimal faces (facing the negative axes).
      pressure: Option<float<kg/(m*s^2)>>;
    }

    member this.is_solid () = match this.media with Solid -> true | _ -> false

    member this.is_not_solid () = not (this.is_solid ())

  // redundant, but allows for slightly cleaner code, especially when filtering.
  let media_is_solid (c: Cell) = c.is_solid ()

  let media_is_not_solid (c: Cell) = c.is_not_solid ()
