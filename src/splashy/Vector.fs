namespace splashy

module Vector =
  type Vector3d =
    struct
      val x: float
      val y: float
      val z: float
      new(x: float, y: float, z:float) = { x = x; y = y; z = z; }
    end

  let add (v1: Vector3d) (v2: Vector3d) = Vector3d(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
  let mult (v: Vector3d) f = Vector3d(v.x * f, v.y * f, v.z * f)

  // partial derivatives
  type Partial = X | Y | Z

  // central difference
  let partial p g (v: Vector3d): float =
    match p with
      | X -> g (Vector3d(v.x + 1.0, v.y, v.z)) - g (Vector3d(v.x - 1.0, v.y, v.z))
      | Y -> g (Vector3d(v.x, v.y + 1.0, v.z)) - g (Vector3d(v.x, v.y - 1.0, v.z))
      | Z -> g (Vector3d(v.x, v.y, v.z + 1.0)) - g (Vector3d(v.x, v.y, v.z - 1.0))

  // forward difference
  let forward p g (v: Vector3d): float =
    match p with
      | X -> g (Vector3d(v.x + 1.0, v.y, v.z)) - g v
      | Y -> g (Vector3d(v.x, v.y + 1.0, v.z)) - g v
      | Z -> g (Vector3d(v.x, v.y, v.z + 1.0)) - g v

  // backward difference
  let backward p g (v: Vector3d): float =
    match p with
      | X -> g v - g (Vector3d(v.x - 1.0, v.y, v.z))
      | Y -> g v - g (Vector3d(v.x, v.y - 1.0, v.z))
      | Z -> g v - g (Vector3d(v.x, v.y, v.z - 1.0))

  // gradient operator
  let gradient g (v: Vector3d) =
    Vector3d(backward Partial.X g v,
             backward Partial.Y g v,
             backward Partial.Z g v)

  // divergence of a vector field
  let divergence ux uy uz (v: Vector3d): float =
    forward Partial.X ux v +
    forward Partial.Y uy v +
    forward Partial.Z uz v

  // Laplacian operator (scalar)
  let laplacian g (v: Vector3d): float =
    forward Partial.X g v + backward Partial.X g v +
    forward Partial.Y g v + backward Partial.Y g v +
    forward Partial.Z g v + backward Partial.Z g v +
    6.0 * g v

  // Laplacian operator (vector)
  let laplacian_vector ux uy uz (v: Vector3d) =
    Vector3d(laplacian ux v,
             laplacian uy v,
             laplacian uz v)
