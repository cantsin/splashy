namespace Splashy

open System
open System.Drawing
open System.Collections.Generic

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

  // configuration.
  let continuous = false
  let draw_debug = true

  // input states.
  let mutable keyPressed = false // don't rush through the simulation.
  let mutable mouseReady = false

  // graphical sub-systems.
  let camera = new Camera ()
  let shader_manager = new ShaderManager ()

  // drawables.
  let world_bounds = new AreaBounds () // fixed, does not change.
  let mutable drawables = []
  let mutable cells = []

  let draw () =
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

    // we draw in a specific order for transparency reasons.
    let air = get_drawables (fun c -> c.media = Air)
    let fluid = get_drawables (fun c -> c.media = Fluid)
    let solids = get_drawables (fun c -> c.media = Solid)

    (world_bounds :> IDrawable).prepare ()
    cells <- fluid @ solids @ air
    for (cell: IDrawable) in cells do
      cell.prepare ()

    drawables <- cells @ [(world_bounds :> IDrawable)]

  override o.OnLoad e =
    o.Cursor <- MouseCursor.Empty

    camera.initialize ()
    shader_manager.initialize ()

    Simulator.advance 0.016671 // 30fps.
    draw ()

    // set other GL states.
    GL.Enable(EnableCap.DepthTest)
    GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)
    GL.Enable(EnableCap.Blend)
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    base.OnLoad e

  override o.OnUnload(e) =
    for drawable in drawables do
      drawable.destroy ()
    shader_manager.unload ()
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
    shader_manager.set_projection projection
    shader_manager.set_model_view <| camera.matrix ()

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
            draw ()
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

    shader_manager.set_model_view <| camera.matrix ()

    for drawable in drawables do
      drawable.render ()

    if draw_debug then
      for drawable in drawables do
        drawable.render_debug ()

    if continuous then
      try
        Simulator.advance e.Time
        draw ()
      with
        | exn ->
          printfn "Exception! %A" exn.Message
          base.Close()

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
