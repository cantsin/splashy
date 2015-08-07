namespace Splashy

open System
open System.Drawing
open System.Collections.Generic
open System.IO

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

open Cell
open Aabb
open Drawables
open Grid
open Simulator
open Camera

type Splashy () =
  inherit GameWindow(800, 600, GraphicsMode.Default, "Splashy")

  do base.VSync <- VSyncMode.On

  let mutable shaders = []
  let mutable programs = []
  let mutable current_program = 0

  // configuration.
  let continuous = false

  let mutable projection_location = 0
  let mutable model_view_location = 0
  let mutable vertex_location = 0 // allow VAOs to set their own matrix transform.

  let mutable keyPressed = false // don't rush through the simulation.

  let camera = new Camera ()
  let mutable mouseReady = false

  // drawables.
  let world_bounds = new AreaBounds () // fixed, does not change.
  let mutable drawables = []
  let mutable cells = []

  let refresh_drawables () =
    for (cell: IDrawable) in cells do
      cell.destroy ()

    let get_drawables fn =
      [ for coord in Grid.filter fn do
        let x = float32 coord.x
        let y = float32 coord.y
        let z = float32 coord.z
        let cell = Grid.raw_get coord
        let cellbounds = new CellBounds(cell)
        cellbounds.set_translation (Vector3(x, y, z))
        yield cellbounds :> IDrawable ]

    let air = get_drawables (fun c -> c.media = Air)
    let fluid = get_drawables (fun c -> c.media = Fluid)
    let solids = get_drawables (fun c -> c.media = Solid)

    (world_bounds :> IDrawable).prepare current_program
    // draw in a specific order for transparency reasons.
    cells <- fluid @ solids @ air
    for (cell: IDrawable) in cells do
      cell.prepare current_program

    drawables <- cells @ [(world_bounds :> IDrawable)]

  let compile_shader shadertype filename =
    let shader = GL.CreateShader(shadertype)
    let source = File.ReadAllText filename
    GL.ShaderSource(shader, source)
    GL.CompileShader(shader)
    shaders <- shader :: shaders
    shader

  override o.OnLoad e =

    o.Cursor <- MouseCursor.Empty

    camera.initialize ()

    let simple_program =
      let program = GL.CreateProgram()
      let vertex_shader = compile_shader ShaderType.VertexShader "src/splashy/shaders/simple.vert"
      let fragment_shader = compile_shader ShaderType.FragmentShader "src/splashy/shaders/simple.frag"
      GL.AttachShader(program, vertex_shader)
      GL.AttachShader(program, fragment_shader)
      GL.LinkProgram(program)
      program
    programs <- simple_program :: programs

    let normal_program =
      let program = GL.CreateProgram()
      let vertex_shader = compile_shader ShaderType.VertexShader "src/splashy/shaders/normal.vert"
      let fragment_shader = compile_shader ShaderType.FragmentShader "src/splashy/shaders/normal.frag"
      let geometry_shader = compile_shader ShaderType.GeometryShader "src/splashy/shaders/normal.geom"
      GL.AttachShader(program, vertex_shader)
      GL.AttachShader(program, fragment_shader)
      GL.AttachShader(program, geometry_shader)
      GL.LinkProgram(program)
      program
    programs <- normal_program :: programs

    current_program <- simple_program

    // check for errors.
    let mutable s = ""
    GL.GetProgramInfoLog(current_program, &s)
    if not (Seq.isEmpty s) then
      failwith (sprintf "Shader compilation failed:\n%A" s)

    GL.UseProgram(current_program)

    projection_location <- GL.GetUniformLocation(current_program, "projectionMatrix")
    model_view_location <- GL.GetUniformLocation(current_program, "modelViewMatrix")
    vertex_location <- GL.GetUniformLocation(current_program, "vertex_mat")

    // set other GL states.
    GL.Enable(EnableCap.DepthTest)
    GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)
    GL.Enable(EnableCap.Blend)
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    refresh_drawables ()

    base.OnLoad e

  override o.OnUnload(e) =
    GL.UseProgram(0)
    for program in programs do
      GL.DeleteProgram(program)
    for shader in shaders do
      GL.DeleteShader(int32 shader)
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
    GL.UniformMatrix4(projection_location, false, &projection)
    let mutable m = camera.matrix ()
    GL.UniformMatrix4(model_view_location, false, &m)

  override o.OnKeyDown e =
    match e.Key with
      | Key.Escape -> base.Close()
      | Key.W -> camera.move (Vector3(0.0f, 0.0f, 1.0f))
      | Key.A -> camera.move (Vector3(1.0f, 0.0f, 0.0f))
      | Key.S -> camera.move (Vector3(0.0f, 0.0f, -1.0f))
      | Key.D -> camera.move (Vector3(-1.0f, 0.0f, 0.0f))
      | Key.Right ->
        if not keyPressed && not continuous then
          try
            Simulator.advance 0.016671 // 30fps.
            refresh_drawables ()
            keyPressed <- true
          with
            | exn ->
              printfn "Exception! %A" exn.Message
              base.Close()
      | _ -> ()

  override o.OnKeyUp e =
    match e.Key with
      | Key.Right -> keyPressed <- false
      | _ -> ()

  override o.OnMouseMove e =
    // hack to ignore first mouse event.
    let x = float base.Width / 2.0
    let y = float base.Height / 2.0
    OpenTK.Input.Mouse.SetPosition(x, y)
    if not mouseReady then
      mouseReady <- true
    else
      let currentX = OpenTK.Input.Mouse.GetState().X
      let currentY = OpenTK.Input.Mouse.GetState().Y
      let diffX = currentX - (int x)
      let diffY = currentY - (int y)
      if diffX <> 0 || diffY <> 0 then
        let yaw = float32 diffX / (float32 base.Width / 2.0f)
        let pitch = float32 diffY / (float32 base.Height / 2.0f)
        camera.rotate yaw pitch

  override o.OnRenderFrame(e) =
    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

    let mutable m = camera.matrix ()
    GL.UniformMatrix4(model_view_location, false, &m)

    if continuous then
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
  let N = 2
  printfn "GL version: %A" version
  printfn "Generating %d random markers" N
  Simulator.generate N
  do splashy.Run(30.0)
