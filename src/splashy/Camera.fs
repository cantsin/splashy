namespace Splashy

open OpenTK

module Camera =

  let eye = Vector3(0.0f, 0.0f, -400.0f)
  let speed = 16.0f

  type Camera () =
    let mutable m = Matrix4.Identity

    member this.initialize () =
      m <- Matrix4.LookAt(eye, Vector3.Zero, Vector3.UnitY)

    member this.move (v: Vector3) =
      m <- m * Matrix4.CreateTranslation(v * speed)

    member this.rotate yaw pitch =
      m <- m * Matrix4.CreateRotationY(yaw)
      m <- m * Matrix4.CreateRotationX(pitch)

    member this.matrix () = m
