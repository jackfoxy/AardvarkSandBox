namespace DslFirstImplementation

open Aardvark.Base
open Aardvark.Application
open FSharp.Data.Adaptive
open Aardvark.SceneGraph

module App =
    // note: this function contains side effects. we just left the functional programming world!
    let createApp (ctrl : IRenderControl) (camera : aval<Camera>) (app : App<'model,'msg, Scene<'msg>>) = 

        let mutable model = app.initial
        let view = AVal.init (app.view model)
        let sceneGraph = view |> AVal.map ConvertToSceneGraph.toSg |> Sg.dynamic

        let updateScene (m : 'model)  =
            let newView = app.view m
            transact (fun _ -> 
                view.Value <- newView
            )

        let handleMouseEvent (createEvent : V3d -> MouseEvent) =
            let ray = ctrl.Mouse.Position |> AVal.force  |> Camera.pickRay (camera |> AVal.force) 
            match Picking.pick ray view.Value with
                | [] -> ()
                | (d,f)::_ -> 
                    for msg in f do
                        match msg (createEvent (ray.GetPointOnRay d)) with
                            | Some r -> model <- app.update model r
                            | _ -> ()
            updateScene model 

        ctrl.Mouse.Move.Values.Add(fun _-> handleMouseEvent MouseEvent.Move) 
        ctrl.Mouse.Down.Values.Add(fun p -> handleMouseEvent ((curry MouseEvent.Down) p))

        sceneGraph
