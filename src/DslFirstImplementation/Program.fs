open Aardvark.Base.Rendering
//open Aardvark.Application.WinForms

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Application
open Aardvark.Application.Slim

open Aardvark.SceneGraph

open DslFirstImplementation.App
open DslFirstImplementation.ConvertToSceneGraph

[<EntryPoint>]
let main argv = 

    Aardvark.Init()

    use app = new OpenGlApplication()
    //let win = app.CreateSimpleRenderWindow()
    let win = app.CreateGameWindow(8)

    let frustum = win.Sizes |> Mod.map (fun s -> Frustum.perspective 60.0 0.1 10.0 (float s.X / float s.Y))
    let cameraView = CameraView.lookAt (V3d.III*3.0) V3d.OOO V3d.OOI
    let camera = 
        frustum |> Mod.map (fun f -> Camera.create cameraView f) 

    let sg = createApp win camera drawingApp

    win.RenderTask <- 
        //win.Runtime.CompileRender(win.FramebufferSignature,
        app.Runtime.CompileRender(win.FramebufferSignature,
            sg 
             |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.simpleLighting |> toEffect]
             |> Sg.camera camera // DefaultSurfaces.simpleLighting needs camera location. by applying the camera here aardvark scenegraph is able to derive it automatically
        )

    win.Run()
    0
