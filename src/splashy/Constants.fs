namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open OpenTK

open Vector

module Constants =

  [<Literal>]
  let h = 10.0<m>
  [<Literal>]
  let fluid_viscosity = 0.000001<m^2/s> // kinematic viscosity
  [<Literal>]
  let fluid_density = 999.97<kg/m^3>
  [<Literal>]
  let atmospheric_pressure = 1013.25<kg/(m*s^2)> // hectopascals
  [<Literal>]
  let max_velocity = 2.5<m/s>
  [<Literal>]
  let time_step_constant = 2.0

  let gravity = Vector3d<m/s^2>(0.0<m/s^2>, -9.81<m/s^2>, 0.0<m/s^2>)

  // pre-calculated.
  let time_step: float<s> = time_step_constant * h / max_velocity

  // world bounds.
  let world_h = 100.0f + (float32 h) / 2.0f
