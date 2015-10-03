namespace Splashy

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
  let mutable continuous = false
  let mutable debug_mode = true

  // input states.
  let mutable keyPressed = false // don't rush through the simulation.
  let mutable mouseReady = false

  // graphical sub-systems.
  let camera = new Camera ()
  let shader_manager = new ShaderManager ()

  // drawables.
  let fluid_color = [|0.2f; 0.2f; 0.8f; 0.7f|]
  let solid_color = [|0.64f; 0.16f; 0.16f; 0.5f|]
  let air_color = [|1.0f; 1.0f; 1.0f; 0.08f|]
  let bounds_color = [|1.0f; 1.0f; 1.0f; 0.1f|]

  let world_bounds = new AreaBounds (bounds_color)
  let fluid_bounds = new CellBounds (fluid_color)
  let solid_bounds = new CellBounds (solid_color)
  let air_bounds = new CellBounds (air_color)

  override o.OnLoad e =
    o.Cursor <- MouseCursor.Empty

    camera.initialize ()
    shader_manager.initialize ()

    world_bounds.prepare ()
    fluid_bounds.prepare ()
    solid_bounds.prepare ()
    air_bounds.prepare ()

    // set other GL states.
    GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.One)
    GL.Enable(EnableCap.Blend)
    GL.Enable(EnableCap.DepthTest)
    GL.LineWidth(2.0f)

    base.OnLoad e

  override o.OnUnload(e) =
    world_bounds.destroy ()
    fluid_bounds.destroy ()
    solid_bounds.destroy ()
    air_bounds.destroy ()
    shader_manager.unload ()
    base.OnUnload e

  override o.OnResize e =
    base.OnResize e
    GL.Viewport(base.ClientRectangle.X,
                base.ClientRectangle.Y,
                base.ClientRectangle.Width,
                base.ClientRectangle.Height)
    shader_manager.set_projection base.Width base.Height
    shader_manager.set_model_view <| camera.matrix ()

  override o.OnKeyDown e =
    match e.Key with
      | Key.Escape -> base.Close()
      | Key.Number0 ->
        debug_mode <- not debug_mode
      | Key.Space ->
        continuous <- not continuous
      | Key.W -> camera.move (Vector3(0.0f, 0.0f, 1.0f))
      | Key.A -> camera.move (Vector3(1.0f, 0.0f, 0.0f))
      | Key.S -> camera.move (Vector3(0.0f, 0.0f, -1.0f))
      | Key.D -> camera.move (Vector3(-1.0f, 0.0f, 0.0f))
      | Key.Right ->
        if not keyPressed && not continuous then
          try
            Simulator.advance 0.016671 // 30fps.
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
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    if continuous then
      try
        Simulator.advance e.Time
      with
        | exn ->
          printfn "Exception! %A" exn.Message
          base.Close()

    // draw the world and all the cells.
    let get_drawables fn =
      [ for coord in Grid.filter fn do
        let x = float32 coord.x
        let y = float32 coord.y
        let z = float32 coord.z
        let c = Grid.raw_get coord
        yield (c, Vector3(x, y, z)) ]

    // we draw in a specific order for transparency reasons.
    let solids = get_drawables (fun c -> c.media = Solid)
    let fluid = get_drawables (fun c -> c.media = Fluid)
    let air = get_drawables (fun c -> c.media = Air)

    // first pass.
    shader_manager.use_main ()
    shader_manager.set_projection base.Width base.Height
    shader_manager.set_model_view <| camera.matrix ()
    for (_, location) in solids do
      solid_bounds.render location
    for (_, location) in fluid do
      fluid_bounds.render location
    for (_, location) in air do
      air_bounds.render location
    world_bounds.render ()

    // second pass (only if debugging).
    if debug_mode then
      GL.Disable(EnableCap.DepthTest)
      shader_manager.use_debug ()
      shader_manager.set_projection base.Width base.Height
      shader_manager.set_model_view <| camera.matrix ()
      for (cell, location) in solids do
        solid_bounds.render_debug cell location
      for (cell, location) in fluid do
        fluid_bounds.render_debug cell location
      for (cell, location) in air do
        air_bounds.render_debug cell location
      GL.Enable(EnableCap.DepthTest)

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
