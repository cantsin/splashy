namespace Splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open OpenTK

open Vector

module Constants =

  [<Literal>]
  let h = 10.0<m>
  [<Literal>]
  let fluid_viscosity = 0.000001<m^2/s> // kinematic viscosity.
  [<Literal>]
  let fluid_density = 999.97<kg/m^3>
  [<Literal>]
  let air_density = 1.2041<kg/m^3>
  [<Literal>]
  //let atmospheric_pressure = 101325.0<kg/(m*s^2)>
  let atmospheric_pressure = 0.0<kg/(m*s^2)>
  [<Literal>]
  let max_velocity = 2.5<m/s>
  [<Literal>]
  let time_step_constant = 2.0

  let gravity = Vector3d<m/s^2>(0.0<_>, -9.81<_>, 0.0<_>)

  // pre-calculated.
  let time_step: float<s> = time_step_constant * h / max_velocity

  // world bounds.
  let world_h = 100.0f + (float32 h) / 2.0f

  let step = 0.2f // how fine grained movement should be

  let N = 1 // number of fluid particles to generate for this system
