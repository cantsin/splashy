namespace splashy

open System
open System.Drawing
open System.Collections.Generic

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

  let vertexSource = """
    #version 330
    precision highp float;
    uniform mat4 projectionMatrix;
    uniform mat4 modelViewMatrix;
    in vec3 vertex_position;
    in vec3 vertex_normal;

    out vec3 normal;
    void main(void)
    {
      gl_Position = projectionMatrix * modelViewMatrix * vec4(vertex_position, 1.0);
      normal = (modelViewMatrix * vec4(vertex_normal, 0)).xyz;
    }
    """

  let fragmentSource = """
    #version 330
    const vec3 ambient = vec3(0.1, 0.1, 0.1);
    const vec3 lightVecNormalized = normalize(vec3(0.5, 0.5, 2));
    const vec3 lightColor = vec3(1.0, 0.8, 0.2);

    in vec3 normal;

    out vec4 outputColor;
    void main(void)
    {
      float diffuse = clamp(dot(lightVecNormalized, normalize(normal)), 0.0, 1.0);
      outputColor = vec4(ambient + diffuse * lightColor, 1.0);
    }
    """

  let mutable vertexShader = 0
  let mutable fragmentShader = 0
  let mutable program = 0
  let mutable vbo = 0
  let mutable indices = 0;
  let mutable normals = 0;

  let mutable projectionLocation = 0
  let mutable modelViewLocation = 0
  let mutable time = 0.0f;

  do base.VSync <- VSyncMode.On

  let positionData = [|Vector3(-0.7f, -0.7f,  0.7f);
                      Vector3( 0.7f, -0.7f,  0.7f);
                      Vector3( 0.7f,  0.7f,  0.7f);
                      Vector3(-0.7f,  0.7f,  0.7f);
                      Vector3(-0.7f, -0.7f, -0.7f);
                      Vector3( 0.7f, -0.7f, -0.7f);
                      Vector3( 0.7f,  0.7f, -0.7f);
                      Vector3(-0.7f,  0.7f, -0.7f)|]
  let indicesData = [|
                     // front face
                     0; 3; 2; 1;
                     // top face
                     3; 2; 6; 7;
                     // back face
                     7; 6; 5; 4;
                     // left face
                     4; 0; 3; 7;
                     // bottom face
                     4; 5; 1; 0;
                     // right face
                     1; 5; 6; 2; |]
  let normalData = [| Vector3(-1.0f, -1.0f,  1.0f);
                    Vector3( 1.0f, -1.0f,  1.0f);
                    Vector3( 1.0f,  1.0f,  1.0f);
                    Vector3(-1.0f,  1.0f,  1.0f);
                    Vector3(-1.0f, -1.0f, -1.0f);
                    Vector3( 1.0f, -1.0f, -1.0f);
                    Vector3( 1.0f,  1.0f, -1.0f);
                    Vector3(-1.0f,  1.0f, -1.0f) |]

  override o.OnLoad e =

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

    let mutable s = ""
    GL.GetProgramInfoLog(program, &s)
    if (Seq.length s) <> 0 then
      failwith (sprintf "Shader compilation failed:\n%A" s)
    GL.UseProgram(program)

    vbo <-
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
      buffer
    GL.BufferData(BufferTarget.ArrayBuffer, nativeint(positionData.Length * Vector3.SizeInBytes), positionData, BufferUsageHint.StaticDraw)
    GL.EnableVertexAttribArray(0)
    GL.BindAttribLocation(vertexShader, 0, "vertex_position")
    GL.VertexAttribPointer(0, 3, VertexAttribPointerType.Float, false, Vector3.SizeInBytes, 0)

    normals <-
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ArrayBuffer, buffer)
      buffer
    GL.BufferData(BufferTarget.ArrayBuffer, nativeint(normalData.Length * Vector3.SizeInBytes), normalData, BufferUsageHint.StaticDraw)
    GL.EnableVertexAttribArray(1)
    GL.BindAttribLocation(vertexShader, 1, "vertex_normal")
    GL.VertexAttribPointer(1, 3, VertexAttribPointerType.Float, false, Vector3.SizeInBytes, 0)

    indices <-
      let buffer = GL.GenBuffer()
      GL.BindBuffer(BufferTarget.ElementArrayBuffer, buffer)
      buffer
    GL.BufferData(BufferTarget.ElementArrayBuffer, nativeint(indicesData.Length * Vector3.SizeInBytes), indicesData, BufferUsageHint.StaticDraw)

    projectionLocation <- GL.GetUniformLocation(program, "projectionMatrix")
    modelViewLocation <- GL.GetUniformLocation(program, "modelViewMatrix")

    // other state
    GL.Enable(EnableCap.DepthTest)
    GL.ClearColor(0.1f, 0.2f, 0.5f, 0.0f)

    base.OnLoad e

  override o.OnUnload(e) =
    GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
    GL.DeleteBuffer(vbo)
    GL.DeleteBuffer(normals)
    GL.DeleteBuffer(indices)

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

    time <- time + (float32)e.Time;
    let mutable lookat = Matrix4.RotateX(time) * Matrix4.RotateY(time) * Matrix4.LookAt(Vector3(0.0f, 0.0f, -4.0f), Vector3.Zero, Vector3.UnitY)
    GL.UniformMatrix4(modelViewLocation, false, &lookat)

    GL.DrawElements(BeginMode.Quads, indicesData.Length, DrawElementsType.UnsignedInt, 0)

    GL.Flush()
    base.SwapBuffers()
    base.OnRenderFrame e

module Library =
  let game = new Game()
  let version = GL.GetString(StringName.Version)
  printfn "GL version %A" version
  do game.Run(30.)
