namespace AardvarkTemplateMedia.Model

//open System
//open Aardvark.Base
open Aardvark.UI.Primitives
open Adaptify

type Primitive =
    | Box
    | Sphere


[<ModelType>]
type Model =
    {
        currentModel    : Primitive
        cameraState     : CameraControllerState
    }