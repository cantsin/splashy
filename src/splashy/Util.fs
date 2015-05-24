namespace splashy

open System

module Util =

  let is_valid_unit f =
    not (Double.IsNaN (f / LanguagePrimitives.FloatWithMeasure 1.0)) &&
    not (Double.IsPositiveInfinity (f / LanguagePrimitives.FloatWithMeasure 1.0)) &&
    not (Double.IsNegativeInfinity (f / LanguagePrimitives.FloatWithMeasure 1.0))

  let filtermap fn seq =
    Seq.map fn seq
    |> Seq.toList
    |> Seq.fold (fun accum elem -> match elem with | Some(item) -> item :: accum | None -> accum)
