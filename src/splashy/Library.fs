namespace splashy

open System
open System.Drawing
open System.Collections.Generic
open System.IO

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

open Aabb
open Drawables
open Grid
open Simulator

type Splashy() =
  inherit GameWindow(800, 600, GraphicsMode.Default, "Splashy")

  do base.VSync <- VSyncMode.On

  let vertex_file = "src/splashy/shaders/simple.vert"
  let fragment_file = "src/splashy/shaders/simple.frag"

  let mutable vertex_shader = 0
  let mutable fragment_shader = 0
  let mutable program = 0

  let mutable projection_location = 0
  let mutable model_view_location = 0
  let mutable vertex_location = 0 // allow VAOs to set their own matrix transform.

  let mutable time = 0.0f
  let mutable pressed = false // don't rush through the simulation.

  let eye = Vector3(0.0f, 0.0f, -400.0f)

  // drawables.
  let world_bounds = new AreaBounds () // fixed, does not change.
  let mutable drawables = []
  let mutable cells = []

  let refresh_drawables () =
    for (cell: IDrawable) in cells do
      cell.destroy ()

    cells <- [ for coord in Grid.filter (fun _ -> true) do
               let x = float32 coord.x
               let y = float32 coord.y
               let z = float32 coord.z
               let cell = Grid.raw_get coord
               let cellbounds = new CellBounds(cell)
               cellbounds.set_translation (Vector3(x, y, z))
               yield cellbounds :> IDrawable ]

    for (cell: IDrawable) in cells do
      cell.prepare program

    // world bound needs to be drawn last (transparency reasons).
    drawables <- cells @ [(world_bounds :> IDrawable)]

  override o.OnLoad e =

    vertex_shader <-
      let shader = GL.CreateShader(ShaderType.VertexShader)
      GL.ShaderSource(shader, File.ReadAllText vertex_file)
      GL.CompileShader(shader)
      shader
    fragment_shader <-
      let shader = GL.CreateShader(ShaderType.FragmentShader)
      GL.ShaderSource(shader, File.ReadAllText fragment_file)
      GL.CompileShader(shader)
      shader
    program <-
      let program = GL.CreateProgram()
      GL.AttachShader(program, vertex_shader)
      GL.AttachShader(program, fragment_shader)
      GL.LinkProgram(program)
      program

    let mutable s = ""
    GL.GetProgramInfoLog(program, &s)
    if not (Seq.isEmpty s) then
      failwith (sprintf "Shader compilation failed:\n%A" s)
    GL.UseProgram(program)

    projection_location <- GL.GetUniformLocation(program, "projectionMatrix")
    model_view_location <- GL.GetUniformLocation(program, "modelViewMatrix")
    vertex_location <- GL.GetUniformLocation(program, "vertex_mat")

    // set other GL states.
    GL.Enable(EnableCap.DepthTest)
    GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One);
    GL.Enable(EnableCap.Blend);
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    refresh_drawables ()
    (world_bounds :> IDrawable).prepare program

    base.OnLoad e

  override o.OnUnload(e) =
    GL.UseProgram(0)
    GL.DeleteProgram(program)
    GL.DeleteShader(vertex_shader)
    GL.DeleteShader(fragment_shader)
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
    GL.UniformMatrix4(projection_location, false, &projection)
    GL.UniformMatrix4(model_view_location, false, &lookat)

  override o.OnKeyDown e =
    match e.Key with
      | Key.Escape -> base.Close()
      | Key.Right ->
        if not pressed then
          try
            Simulator.advance 0.016671 // 30fps.
            refresh_drawables ()
            pressed <- true
          with
            | exn ->
              printfn "Exception! %A" exn.Message
              base.Close()
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
    GL.UniformMatrix4(model_view_location, false, &lookat)

    try
      Simulator.advance e.Time
      refresh_drawables ()
    with
      | exn ->
        printfn "Exception! %A" exn.Message
        base.Close()

    for drawable in drawables do
      drawable.render vertex_location

    let code = GL.GetError ()
    if code <> ErrorCode.NoError then
      printfn "GL error! %A" code
      base.Close()

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
