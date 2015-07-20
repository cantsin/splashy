namespace Splashy

open OpenTK

module Camera =

  let eye = Vector3(0.0f, 0.0f, -400.0f)

  type Camera () =
    let mutable m = Matrix4.Identity

    member this.initialize () =
      m <- Matrix4.LookAt(eye, Vector3.Zero, Vector3.UnitY)

    member this.move (v: Vector3) =
      let v4 = Vector4(v, 1.0f)
      let t = Vector4.Transform (v4, m)
      m.Row3 <- t

    member this.rotate angle =
      m <- m * Matrix4.CreateRotationY(angle)
      m * Matrix4.LookAt(eye, Vector3.Zero, Vector3.UnitY)

    member this.matrix () = m
