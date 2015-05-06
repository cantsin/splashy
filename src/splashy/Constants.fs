namespace splashy

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

open Vector
open Aabb

module Constants =

  [<Literal>]
  let h = 10.0
  let h_si = 10.0<m>

  [<Literal>]
  let fluid_viscosity = 0.000001 // kinematic viscosity
  let fluid_viscosity_si = 0.000001<m^2/s>
  [<Literal>]
  let fluid_density = 999.97
  let fluid_density_si = 999.97<kg/m^3>
  [<Literal>]
  let atmospheric_pressure = 1013.25 // hectopascals (100 kg/(m * s^2))
  let atmospheric_pressure_si = 101300.25<kg/(m*s^2)>

  [<Literal>]
  let max_velocity = 2.5
  let max_velocity_si = 2.5<m/s>

  let gravity = Vector3d(0.0, -9.81, 0.0) // m/s^2

  [<Literal>]
  let time_step_constant = 2.5

  // pre-calculated.
  let time_step = time_step_constant * h / max_velocity
  let time_step_si: float<s> = time_step_constant * h_si / max_velocity_si

  // the world bounding box.
  let bounds_h = 100.0 + h / 2.0
  let bounds = { min_bounds = Vector3d(-bounds_h, -bounds_h, -bounds_h);
                 max_bounds = Vector3d( bounds_h,  bounds_h,  bounds_h) }
