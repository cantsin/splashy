namespace splashy

open System
open System.Drawing
open System.Collections.Generic
open System.IO

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

open Vector
open Quad
open Aabb
open Simulator

type Game() =
  inherit GameWindow(800, 600, GraphicsMode.Default, "Splashy")

  let vertexFile = "src/splashy/shaders/simple.vert"
  let fragmentFile = "src/splashy/shaders/simple.frag"

  let mutable vertexShader = 0
  let mutable fragmentShader = 0
  let mutable program = 0
  let mutable vaos = []

  let mutable projectionLocation = 0
  let mutable modelViewLocation = 0
  let mutable time = 0.0f

  do base.VSync <- VSyncMode.On

  let samples = [{ min_bounds = Vector3d(-0.5, -0.5, -0.5);
                   max_bounds = Vector3d(-0.3, -0.3, -0.3) };
                 { min_bounds = Vector3d(-0.8, -0.8, -0.8);
                   max_bounds = Vector3d(-0.7, -0.7, -0.7) };
                 { min_bounds = Vector3d(-0.1, -0.1, -0.1);
                   max_bounds = Vector3d( 0.0,  0.0,  0.0) }]

  let add_aabb (data: float []) =
    let mutable vao =
      let array = GL.GenVertexArray()
      GL.BindVertexArray(array)
      array

    let mutable vbo =
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
      buffer
    let vertexPosition = GL.GetAttribLocation(program, "vertex_position")
    GL.BufferData(BufferTarget.ArrayBuffer, nativeint(data.Length * 4 * sizeof<float>), data, BufferUsageHint.StaticDraw)
    GL.EnableVertexAttribArray(vertexPosition)
    GL.BindAttribLocation(vertexShader, vertexPosition, "vertex_position")
    GL.VertexAttribPointer(vertexPosition, 4, VertexAttribPointerType.Double, false, 4 * sizeof<float>, 0)

    let mutable normals =
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
      buffer
    let vertexNormal = GL.GetAttribLocation(program, "vertex_normal")
    GL.BufferData(BufferTarget.ArrayBuffer, nativeint(Aabb.normalData.Length * 4 * sizeof<float32>), Aabb.normalData, BufferUsageHint.StaticDraw)
    GL.EnableVertexAttribArray(vertexNormal)
    GL.BindAttribLocation(vertexShader, vertexNormal, "vertex_normal")
    GL.VertexAttribPointer(vertexNormal, 4, VertexAttribPointerType.Float, false, 4 * sizeof<float32>, 0)

    let mutable indices =
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

    vaos <- vao :: vaos

  override o.OnLoad e =

    vertexShader <-
      let shader = GL.CreateShader(ShaderType.VertexShader)
      GL.ShaderSource(shader, File.ReadAllText vertexFile)
      GL.CompileShader(shader)
      shader
    fragmentShader <-
      let shader = GL.CreateShader(ShaderType.FragmentShader)
      GL.ShaderSource(shader, File.ReadAllText fragmentFile)
      GL.CompileShader(shader)
      shader
    program <-
      let program = GL.CreateProgram()
      GL.AttachShader(program, vertexShader)
      GL.AttachShader(program, fragmentShader)
      GL.LinkProgram(program)
      program

    let mutable s = ""
    GL.GetProgramInfoLog(program, &s)
    if (Seq.length s) <> 0 then
      failwith (sprintf "Shader compilation failed:\n%A" s)
    GL.UseProgram(program)

    projectionLocation <- GL.GetUniformLocation(program, "projectionMatrix")
    modelViewLocation <- GL.GetUniformLocation(program, "modelViewMatrix")

    // testing purposes.
    for sample in samples do
      Aabb.rawData sample |> add_aabb

    // other state
    GL.Enable(EnableCap.DepthTest)
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    base.OnLoad e

  override o.OnUnload(e) =

    GL.UseProgram(0)
    GL.DeleteProgram(program)
    GL.DeleteShader(vertexShader)
    GL.DeleteShader(fragmentShader)
    base.OnUnload e

  override o.OnResize e =
    base.OnResize e
    GL.Viewport(base.ClientRectangle.X,
                base.ClientRectangle.Y,
                base.ClientRectangle.Width,
                base.ClientRectangle.Height)
    let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI / 4.0),
                                                                  float32 base.Width / float32 base.Height,
                                                                  1.f,
                                                                  64.f)
    let mutable lookat = Matrix4.LookAt(Vector3(0.0f, 0.0f, -4.0f), Vector3.Zero, Vector3.UnitY)
    GL.UniformMatrix4(projectionLocation, false, &projection)
    GL.UniformMatrix4(modelViewLocation, false, &lookat)

  override o.OnUpdateFrame e =
    base.OnUpdateFrame e
    if base.Keyboard.[Key.Escape] then base.Close()
    if base.Keyboard.[Key.Right] then Simulator.advance ()

  override o.OnRenderFrame(e) =
    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

    time <- time + (float32)e.Time
    let mutable lookat = Matrix4.RotateX(time) * Matrix4.RotateY(time) * Matrix4.RotateZ(time) * Matrix4.LookAt(Vector3(0.0f, 0.0f, -4.0f), Vector3.Zero, Vector3.UnitY)
    GL.UniformMatrix4(modelViewLocation, false, &lookat)

    for vao in vaos do
      GL.BindVertexArray(vao)
      GL.DrawElements(BeginMode.Quads, indicesData.Length, DrawElementsType.UnsignedInt, 0)
      GL.BindVertexArray(0)

    GL.Flush()
    base.SwapBuffers()
    base.OnRenderFrame e

module Library =
  let game = new Game()
  let version = GL.GetString(StringName.Version)
  printfn "GL version: %A" version
  do game.Run(30.)
