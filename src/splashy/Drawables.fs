namespace splashy

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open Aabb
open Grid
open Simulator

type IDrawable =
  abstract member prepare: int -> int -> unit
  abstract member render: int -> unit
  abstract member destroy: unit -> unit

// encapsulate GL calls to draw the bounding area, markers, and cells.
module Drawables =

  let prepare_aabb (program: int) (vs: int) (data: float []) =
    let vao =
      let array = GL.GenVertexArray()
      GL.BindVertexArray(array)
      array

    let vbo =
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
      buffer
    let vertexPosition = GL.GetAttribLocation(program, "vertex_position")
    GL.BufferData(BufferTarget.ArrayBuffer, nativeint(data.Length * 4 * sizeof<float>), data, BufferUsageHint.StaticDraw)
    GL.EnableVertexAttribArray(vertexPosition)
    GL.BindAttribLocation(vs, vertexPosition, "vertex_position")
    GL.VertexAttribPointer(vertexPosition, 4, VertexAttribPointerType.Double, false, 4 * sizeof<float>, 0)

    let normals =
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
      buffer
    let vertexNormal = GL.GetAttribLocation(program, "vertex_normal")
    GL.BufferData(BufferTarget.ArrayBuffer, nativeint(Aabb.normalData.Length * 4 * sizeof<float32>), Aabb.normalData, BufferUsageHint.StaticDraw)
    GL.EnableVertexAttribArray(vertexNormal)
    GL.BindAttribLocation(vs, vertexNormal, "vertex_normal")
    GL.VertexAttribPointer(vertexNormal, 4, VertexAttribPointerType.Float, false, 4 * sizeof<float32>, 0)

    let indices =
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ElementArrayBuffer, buffer)
      buffer
    GL.BufferData(BufferTarget.ElementArrayBuffer, nativeint(Aabb.indicesData.Length * 4 * sizeof<int>), Aabb.indicesData, BufferUsageHint.StaticDraw)

    GL.BindVertexArray(0)
    GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
    GL.BindBuffer(BufferTarget.ElementArrayBuffer, 0)
    GL.DisableVertexAttribArray(vertexPosition)
    GL.DisableVertexAttribArray(vertexNormal)
    GL.DeleteBuffer(vbo)
    GL.DeleteBuffer(normals)
    GL.DeleteBuffer(indices)

    vao

  type AreaBounds () =
    let mutable vao = 0
    let mutable position = Matrix4.Identity
    interface IDrawable with
      member this.prepare p vs =
        vao <- prepare_aabb p vs (Aabb.rawData Simulator.bounds)
      member this.render location =
        GL.UniformMatrix4(location, false, &position)
        GL.BindVertexArray(vao)
        GL.DrawElements(BeginMode.Quads, indicesData.Length, DrawElementsType.UnsignedInt, 0)
        GL.BindVertexArray(0)
      member this.destroy () =
        GL.DeleteVertexArray(vao)

  type CellBounds () =
    let cell = { min_bounds = Vector.Vector3d(-Grid.h/2.0, -Grid.h/2.0, -Grid.h/2.0);
                 max_bounds = Vector.Vector3d( Grid.h/2.0,  Grid.h/2.0,  Grid.h/2.0) }
    let mutable vao = 0
    let mutable vertex_mat = 0
    let mutable transform = Vector3.Zero
    interface IDrawable with
      member this.prepare p vs =
        vertex_mat <- GL.GetUniformLocation(p, "vertex_mat")
        let newVao = prepare_aabb p vs (Aabb.rawData cell)
        vao <- newVao
      member this.render location =
        let mutable m = Matrix4.CreateTranslation(transform)
        GL.UniformMatrix4(location, false, &m)
        GL.BindVertexArray(vao)
        GL.DrawElements(BeginMode.Quads, indicesData.Length, DrawElementsType.UnsignedInt, 0)
        GL.BindVertexArray(0)
      member this.destroy () =
        GL.DeleteVertexArray(vao)
    member this.set_translation t = transform <- t
