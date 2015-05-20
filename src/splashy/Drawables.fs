namespace splashy

open System.Linq

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open World
open Cell
open Constants
open Aabb
open Coord
open Grid
open Simulator

type IDrawable =
  abstract member prepare: int -> unit
  abstract member render: int -> unit
  abstract member destroy: unit -> unit

// encapsulate GL calls to draw the bounding area, markers, and cells.
module Drawables =

  let internal prepare_aabb (program: int) color_data (raw_data: float32 []) =
    let vao =
      let array = GL.GenVertexArray()
      GL.BindVertexArray(array)
      array

    let create_buffer (data: float32 []) attrib =
      let new_buffer =
        let buffer = GL.GenBuffer()
        GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
        buffer
      let location = GL.GetAttribLocation(program, attrib)
      let n = sizeof<float32>
      GL.BufferData(BufferTarget.ArrayBuffer, nativeint(data.Length * n), data, BufferUsageHint.StaticDraw)
      GL.EnableVertexAttribArray(location)
      GL.VertexAttribPointer(location, 4, VertexAttribPointerType.Float, false, n * 4, 0)
      GL.BindAttribLocation(program, location, attrib)
      (location, new_buffer)

    let (vertex_position, vbo) = create_buffer raw_data "vertex_position"
    let (vertex_normal, normals) = create_buffer Aabb.normal_data "vertex_normal"

    // repeat the color for each element in raw_data
    let length = raw_data.Length / 4 + 1
    let color_data = Seq.collect Enumerable.Repeat [ color_data, length ] |> Seq.concat |> Array.ofSeq
    let (vertex_color, colors) = create_buffer color_data "vertex_color"

    let indices =
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ElementArrayBuffer, buffer)
      buffer
    let n = sizeof<int>
    let data = Aabb.indices_data
    GL.BufferData(BufferTarget.ElementArrayBuffer, nativeint(data.Length * n), data, BufferUsageHint.StaticDraw)

    GL.BindVertexArray(0)
    GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
    GL.BindBuffer(BufferTarget.ElementArrayBuffer, 0)
    GL.DisableVertexAttribArray(vertex_color)
    GL.DisableVertexAttribArray(vertex_normal)
    GL.DisableVertexAttribArray(vertex_position)
    GL.DeleteBuffer(normals)
    GL.DeleteBuffer(indices)
    GL.DeleteBuffer(colors)
    GL.DeleteBuffer(vbo)

    vao

  type AreaBounds () =
    let mutable vao = 0
    let mutable position = Matrix4.Identity
    let color = [|1.0f; 1.0f; 1.0f; 0.1f|]
    interface IDrawable with
      member this.prepare p =
        vao <- prepare_aabb p color (Aabb.raw_data World.bounds)
      member this.render location =
        GL.UniformMatrix4(location, false, &position)
        GL.BindVertexArray(vao)
        GL.DrawElements(BeginMode.Quads, indices_data.Length, DrawElementsType.UnsignedInt, 0)
        GL.BindVertexArray(0)
      member this.destroy () =
        GL.DeleteVertexArray(vao)

  type CellBounds (cell) =
    let mutable data = cell
    let mutable vao = 0
    let mutable transform = Vector3.Zero
    let fluid_color = [|0.2f; 0.2f; 0.8f; 0.7f|]
    let solid_color = [|0.64f; 0.16f; 0.16f; 0.5f|]
    let air_color = [|1.0f; 1.0f; 1.0f; 0.01f|]
    let l = float32 Constants.h / 2.0f
    let cell_bounds = { min_bounds = Vector3(-l, -l, -l);
                        max_bounds = Vector3( l,  l,  l) }
    interface IDrawable with
      member this.prepare p =
        let color = match cell.media with
                       | Fluid -> fluid_color
                       | Solid -> solid_color
                       | Air -> air_color
        let new_vao = prepare_aabb p color (Aabb.raw_data <| Aabb.fudge cell_bounds)
        vao <- new_vao
      member this.render location =
        let mutable m = Matrix4.CreateTranslation(transform)
        GL.UniformMatrix4(location, false, &m)
        GL.BindVertexArray(vao)
        GL.DrawElements(BeginMode.Quads, indices_data.Length, DrawElementsType.UnsignedInt, 0)
        GL.BindVertexArray(0)
      member this.destroy () =
        GL.DeleteVertexArray(vao)
    member this.set_translation t = transform <- t
