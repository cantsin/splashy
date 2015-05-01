namespace splashy

module Vector =
  type Vector3d =
    struct
      val x: float
      val y: float
      val z: float

      new(x: float, y: float, z:float) = { x = x; y = y; z = z; }

      // set up explicit operators, as i dislike overloading common operators.
      static member (.+) (v1: Vector3d, v2: Vector3d) = Vector3d(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)

      static member (.-) (v1: Vector3d, v2: Vector3d) = Vector3d(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)

      static member (.*) (v: Vector3d, f) = Vector3d(v.x * f, v.y * f, v.z * f)

      static member dot (v1: Vector3d) (v2: Vector3d) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z

      static member cross (v1: Vector3d) (v2: Vector3d) =
        Vector3d(v1.y * v2.z - v1.z * v2.y,
                 v1.z * v2.x - v1.x * v2.z,
                 v1.x * v2.y - v1.y * v2.x)

      static member min (v1: Vector3d) (v2: Vector3d) = Vector3d(min v1.x v2.x,
                                                                 min v1.y v2.y,
                                                                 min v1.z v2.z)

      static member max (v1: Vector3d) (v2: Vector3d) = Vector3d(max v1.x v2.x,
                                                                 max v1.y v2.y,
                                                                 max v1.z v2.z)

      override this.ToString () = sprintf "Vector3d[%f; %f; %f]" this.x this.y this.z
    end
