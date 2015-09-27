namespace Splashy

open System
open System.Linq
open System.IO

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

// encapsulate GL calls to draw the bounding area, markers, and cells.
module Drawables =

  let mutable main_program = 0
  let mutable debug_program = 0
  let mutable vertex_location = 0
  let mutable projection_location = 0
  let mutable model_view_location = 0

  // helper function
  let check_program (program: int) =
    let mutable s = ""
    GL.GetProgramInfoLog(program, &s)
    if not (Seq.isEmpty s) then
      failwith (sprintf "Could not use shader program:\n%A" s)

  // helper function
  let use_program (program: int) =
    GL.UseProgram(program)
    if program <> 0 then
      projection_location <- GL.GetUniformLocation(program, "projection_matrix")
      model_view_location <- GL.GetUniformLocation(program, "model_view_matrix")
      vertex_location <- GL.GetUniformLocation(program, "vertex_matrix")

  type ShaderManager () =

    let mutable shaders = []

    let compile_shader shadertype filename =
      let shader = GL.CreateShader(shadertype)
      let source = File.ReadAllText filename
      GL.ShaderSource(shader, source)
      GL.CompileShader(shader)
      shaders <- shader :: shaders
      shader

    member this.initialize () =

      debug_program <-
        let program = GL.CreateProgram()
        let vertex_shader = compile_shader ShaderType.VertexShader "src/splashy/shaders/debug.vert"
        let geometry_shader = compile_shader ShaderType.GeometryShader "src/splashy/shaders/debug.geom"
        let fragment_shader = compile_shader ShaderType.FragmentShader "src/splashy/shaders/debug.frag"
        GL.AttachShader(program, vertex_shader)
        GL.AttachShader(program, geometry_shader)
        GL.AttachShader(program, fragment_shader)
        GL.LinkProgram(program)
        check_program program
        program

      main_program <-
        let program = GL.CreateProgram()
        let vertex_shader = compile_shader ShaderType.VertexShader "src/splashy/shaders/simple.vert"
        let fragment_shader = compile_shader ShaderType.FragmentShader "src/splashy/shaders/simple.frag"
        GL.AttachShader(program, vertex_shader)
        GL.AttachShader(program, fragment_shader)
        GL.LinkProgram(program)
        check_program program
        program

      use_program main_program

    member this.set_projection width height =
      let fovy = float32 (Math.PI / 4.0)
      let aspect = float32 width / float32 height
      let mutable m = Matrix4.CreatePerspectiveFieldOfView(fovy, aspect, 1.0f, 640.0f)
      GL.UniformMatrix4(projection_location, false, &m)

    member this.set_model_view (model_view: Matrix4) =
      let mutable m = model_view
      GL.UniformMatrix4(model_view_location, false, &m)

    member this.use_debug () = use_program debug_program
    member this.use_main () = use_program main_program

    member this.unload () =
      GL.UseProgram(0)
      GL.DeleteProgram(main_program)
      GL.DeleteProgram(debug_program)
      for shader in shaders do
        GL.DeleteShader(int32 shader)

  // helper function
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

  // helper function
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

  // helper function
  let repeat_seq (s: seq<'a>) (l: int) =
    Seq.collect Enumerable.Repeat [ s, l ] |> Seq.concat |> Array.ofSeq

  type AreaBounds (color) =
    let mutable vao = 0
    let vertex_data = Aabb.raw_data World.bounds
    let color_data = repeat_seq color (vertex_data.Length / 4 + 1)
    let attributes = [("vertex_position", vertex_data);
                      ("vertex_normal", Aabb.normal_data);
                      ("vertex_color", color_data)]
    member this.prepare () =
      vao <- create_vao main_program attributes Aabb.indices_data
    member this.render () =
      let mutable m = Matrix4.Identity
      GL.UniformMatrix4(vertex_location, false, &m)
      GL.BindVertexArray(vao)
      GL.DrawElements(BeginMode.Triangles, Aabb.indices_data.Length, DrawElementsType.UnsignedInt, 0)
      GL.BindVertexArray(0)
    member this.destroy () =
      GL.DeleteVertexArray(vao)

  type CellBounds (color) =
    let mutable main_vao = 0
    let mutable debug_vao = 0
    let l = float32 Constants.h / 2.0f
    let cell_bounds = { min_bounds = Vector3(-l, -l, -l);
                        max_bounds = Vector3( l,  l,  l) }
    let vertex_data = Aabb.raw_data <| Aabb.fudge cell_bounds
    let color_data = repeat_seq color (vertex_data.Length / 4 + 1)
    let attributes = [("vertex_position", vertex_data);
                      ("vertex_normal", Aabb.normal_data);
                      ("vertex_color", color_data)]
    let debug_attributes = [("vertex_position", vertex_data)]
    member this.prepare () =
      main_vao <- create_vao main_program attributes Aabb.indices_data
      debug_vao <- create_vao debug_program debug_attributes Aabb.indices_data
    member this.render where debug =
      let mutable m = Matrix4.CreateTranslation(where)
      GL.UniformMatrix4(vertex_location, false, &m)
      GL.BindVertexArray(if debug then debug_vao else main_vao)
      GL.DrawElements(BeginMode.Triangles, Aabb.indices_data.Length, DrawElementsType.UnsignedInt, 0)
      GL.BindVertexArray(0)
    member this.destroy () =
      GL.DeleteVertexArray(main_vao)
      GL.DeleteVertexArray(debug_vao)
