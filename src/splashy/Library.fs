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
open Grid
open Simulator

type Splashy() =
  inherit GameWindow(800, 600, GraphicsMode.Default, "Splashy")

  do base.VSync <- VSyncMode.On

  let vertexFile = "src/splashy/shaders/simple.vert"
  let fragmentFile = "src/splashy/shaders/simple.frag"

  let mutable vertexShader = 0
  let mutable fragmentShader = 0
  let mutable program = 0

  let mutable projectionLocation = 0
  let mutable modelViewLocation = 0
  let mutable vertexLocation = 0 // allow VAOs to set their own matrix transform

  let mutable time = 0.0f
  let mutable pressed = false // don't rush through simulation

  let eye = Vector3(0.0f, 0.0f, -400.0f)

  // drawables.
  let worldBounds = new AreaBounds () // fixed, does not change
  let mutable drawables = []
  let mutable cells = []

  let refresh_drawables () =
    for (cell: IDrawable) in cells do
      cell.destroy ()

    cells <- [ for (KeyValue(coord, cell)) in Grid.grid do
               let x = float32 coord.x
               let y = float32 coord.y
               let z = float32 coord.z
               let cellbounds = new CellBounds(cell)
               cellbounds.set_translation (Vector3(x, y, z))
               yield cellbounds :> IDrawable ]

    for (cell: IDrawable) in cells do
      cell.prepare program vertexShader

    // world bound needs to be drawn last (transparency reasons)
    drawables <- cells @ [(worldBounds :> IDrawable)]

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

    // set other GL states.
    GL.Enable(EnableCap.DepthTest)
    GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One);
    GL.Enable(EnableCap.Blend);
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    refresh_drawables ()
    (worldBounds :> IDrawable).prepare program vertexShader

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
                                                                  1.0f,
                                                                  640.0f)
    let mutable lookat = Matrix4.LookAt(eye, Vector3.Zero, Vector3.UnitY)
    GL.UniformMatrix4(projectionLocation, false, &projection)
    GL.UniformMatrix4(modelViewLocation, false, &lookat)

  override o.OnKeyDown e =
    match e.Key with
      | Key.Escape -> base.Close()
      | Key.Right ->
        if not pressed then
          Simulator.advance ()
          refresh_drawables ()
          pressed <- true
      | _ -> ()

  override o.OnKeyUp e =
    match e.Key with
      | Key.Right -> pressed <- false
      | _ -> ()

  override o.OnRenderFrame(e) =
    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

    time <- time + (float32)e.Time
    let rot = Matrix4.CreateRotationY(time)
    let mutable lookat = rot * Matrix4.LookAt(eye, Vector3.Zero, Vector3.UnitY)
    GL.UniformMatrix4(modelViewLocation, false, &lookat)

    for drawable in drawables do
      drawable.render vertexLocation

    GL.Flush()
    base.SwapBuffers()
    base.OnRenderFrame e

module Library =
  let splashy = new Splashy()
  let version = GL.GetString(StringName.Version)
  let N = 10
  printfn "GL version: %A" version
  printfn "Generating %d random markers" N
  Simulator.generate N
  do splashy.Run(30.0)
