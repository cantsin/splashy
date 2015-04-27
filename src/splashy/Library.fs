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
open Aabb
open Drawables
open Simulator

type Game() =
  inherit GameWindow(800, 600, GraphicsMode.Default, "Splashy")

  let vertexFile = "src/splashy/shaders/simple.vert"
  let fragmentFile = "src/splashy/shaders/simple.frag"

  let mutable vertexShader = 0
  let mutable fragmentShader = 0
  let mutable program = 0
  let mutable drawables = []

  let mutable projectionLocation = 0
  let mutable modelViewLocation = 0
  let mutable vertexLocation = 0 // allow VAOs to set their own matrix transform
  let mutable time = 0.0f

  do base.VSync <- VSyncMode.On

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
    vertexLocation <- GL.GetUniformLocation(program, "vertex_mat")

    let bounds = new BoundingArea ()
    drawables <- (bounds :> IDrawable) :: drawables
    let r = System.Random()
    for i in 0..9 do
      let cell = new Cell ()
      let x = (float32 (r.Next(-i, i))) / 10.0f
      let y = (float32 (r.Next(-i, i))) / 10.0f
      let z = (float32 (r.Next(-i, i))) / 10.0f
      cell.set_translation (Vector3(x, y, z))
      drawables <- (cell :> IDrawable) :: drawables

    // initialize.
    for drawable in drawables do
      drawable.prepare program vertexShader

    // set other GL states.
    GL.Enable(EnableCap.DepthTest)
    GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One);
    GL.Enable(EnableCap.Blend);
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    base.OnLoad e

  override o.OnUnload(e) =
    GL.UseProgram(0)
    GL.DeleteProgram(program)
    GL.DeleteShader(vertexShader)
    GL.DeleteShader(fragmentShader)
    for drawable in drawables do
      drawable.destroy ()
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

    for drawable in drawables do
      drawable.render vertexLocation

    GL.Flush()
    base.SwapBuffers()
    base.OnRenderFrame e

module Library =
  let game = new Game()
  let version = GL.GetString(StringName.Version)
  printfn "GL version: %A" version
  do game.Run(30.)
