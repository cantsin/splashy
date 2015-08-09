namespace Splashy

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

  let internal create_attrib_buffer (program: int) (attrib: string) (data: float32 []) =
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

  let internal create_vao (program: int) (attributes: seq<string * float32 []>) (indices: int []) =
    let vao =
      let array = GL.GenVertexArray()
      GL.BindVertexArray(array)
      array
    let result = [for (name, data) in attributes do yield create_attrib_buffer program name data]
    let index_buffer =
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ElementArrayBuffer, buffer)
      buffer
    let n = sizeof<int>
    GL.BufferData(BufferTarget.ElementArrayBuffer, nativeint(indices.Length * n), indices, BufferUsageHint.StaticDraw)

    // clean-up.
    GL.BindVertexArray(0)
    GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
    GL.BindBuffer(BufferTarget.ElementArrayBuffer, 0)
    for (attrib, buffer) in result do
      GL.DisableVertexAttribArray(attrib)
      GL.DeleteBuffer(buffer)
    GL.DeleteBuffer(index_buffer)

    vao

  type AreaBounds () =
    let mutable vao = 0
    let mutable position = Matrix4.Identity
    let color = [|1.0f; 1.0f; 1.0f; 0.1f|]
    interface IDrawable with
      member this.prepare p =
        let vertex_data = Aabb.raw_data World.bounds
        let color_data = Seq.collect Enumerable.Repeat [ color, vertex_data.Length / 4 + 1 ] |> Seq.concat |> Array.ofSeq
        let attributes = [("vertex_position", vertex_data);
                          ("vertex_normal", Aabb.normal_data);
                          ("vertex_color", color_data)]
        vao <- create_vao p attributes Aabb.indices_data
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
        let vertex_data = Aabb.raw_data <| Aabb.fudge cell_bounds
        let color_data = Seq.collect Enumerable.Repeat [ color, vertex_data.Length / 4 + 1 ] |> Seq.concat |> Array.ofSeq
        let attributes = [("vertex_position", vertex_data);
                          ("vertex_normal", Aabb.normal_data);
                          ("vertex_color", color_data)]
        vao <- create_vao p attributes Aabb.indices_data
      member this.render location =
        let mutable m = Matrix4.CreateTranslation(transform)
        GL.UniformMatrix4(location, false, &m)
        GL.BindVertexArray(vao)
        GL.DrawElements(BeginMode.Quads, indices_data.Length, DrawElementsType.UnsignedInt, 0)
        GL.BindVertexArray(0)
      member this.destroy () =
        GL.DeleteVertexArray(vao)
    member this.set_translation t = transform <- t
