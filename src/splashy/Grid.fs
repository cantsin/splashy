namespace splashy

open System.Collections.Generic

open Vector

module Grid =
  type Media = Air | Water | Solid
  type Cell = { pressure: float;
                media: Media;
                velocity: Vector3d; // from the minimal faces, not the center
                level: int; }

  // configuration options.
  let h = 1.0
  let time_step_constant = 2.5

  let mutable grid = new Dictionary<int, Cell>()

  let hash x y z = 541 * x + 79 * y + 31 * z

  let add_to_grid x y z c = grid.Add (hash x y z, c)
