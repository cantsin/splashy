namespace Splashy

module Forces =

  // gravity only (for now).
  let apply markers dt =
    let f = Constants.gravity .* dt
    Seq.map (fun m ->
               let c = Grid.raw_get m
               let v = c.velocity .+ f
               (m, v)
            ) markers
