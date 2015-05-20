namespace splashy

module Forces =

  // gravity only (for now).
  let apply_forces markers dt =
    let f = Constants.gravity .* dt
    for marker in markers do
      let c = Grid.raw_get marker
      Grid.set marker { c with velocity = c.velocity .+ f }
