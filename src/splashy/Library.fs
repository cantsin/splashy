namespace splashy

open System
open System.Drawing
open System.Collections.Generic

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

open Vector

type Game() =
  inherit GameWindow(800, 600, GraphicsMode.Default, "Splashy")

  let vertexSource = """
    #version 330
    layout (location = 0) in vec4 position;
    void main(void)
    {
      gl_Position = position;
    }
    """

  let fragmentSource = """
    #version 330
    out vec4 outputColor;
    void main(void)
    {
      outputColor = vec4(1.0, 0.0, 0.0, 1.0);
    }
    """

  let points = [| -0.5f; 0.0f; 0.0f; 1.0f; 0.5f; 0.0f; 0.0f; 1.0f; 0.0f; 0.5f; 0.0f; 1.0f |]

  let mutable vertexShader = 0
  let mutable fragmentShader = 0
  let mutable program = 0
  let mutable verticesVbo = 0
  let mutable vao = 0

  do base.VSync <- VSyncMode.On

  override o.OnLoad e =
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)
    GL.Enable(EnableCap.DepthTest)
    vertexShader <-
      let shader = GL.CreateShader(ShaderType.VertexShader)
      GL.ShaderSource(shader, vertexSource)
      GL.CompileShader(shader)
      shader
    fragmentShader <-
      let shader = GL.CreateShader(ShaderType.FragmentShader)
      GL.ShaderSource(shader, fragmentSource)
      GL.CompileShader(shader)
      shader
    program <-
      let program = GL.CreateProgram()
      GL.AttachShader(program, vertexShader)
      GL.AttachShader(program, fragmentShader)
      GL.LinkProgram(program)
      program
    verticesVbo <-
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
      buffer
    vao <-
      let array = GL.GenVertexArray()
      GL.BindVertexArray(array)
      array

    // Transfer the vertices from CPU to GPU.
    GL.BufferData(BufferTarget.ArrayBuffer, nativeint(3 * 4 * sizeof<float32>), points, BufferUsageHint.StaticDraw)
    GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
    GL.UseProgram(program)

    let vertexPosition = GL.GetAttribLocation(program, "position")
    GL.BindBuffer(BufferTarget.ArrayBuffer, verticesVbo)
    GL.VertexAttribPointer(vertexPosition, 4, VertexAttribPointerType.Float, false, 0, 0)
    GL.EnableVertexAttribArray(vertexPosition)

    base.OnLoad e

  override this.OnUnload(e) =
    GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
    GL.DeleteBuffer(verticesVbo)
    GL.DeleteVertexArray(vao)

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
    let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI / 4.),
                                                                  float32 base.Width / float32 base.Height,
                                                                  1.f,
                                                                  64.f)
    GL.MatrixMode(MatrixMode.Projection)
    GL.LoadMatrix(&projection)

  override o.OnUpdateFrame e =
    base.OnUpdateFrame e
    if base.Keyboard.[Key.Escape] then base.Close()

  override o.OnRenderFrame(e) =
    GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

    GL.DrawArrays(BeginMode.Triangles, 0, 3);

    base.SwapBuffers()
    base.OnRenderFrame e

module Library =
  let game = new Game()
  do game.Run(30.)
