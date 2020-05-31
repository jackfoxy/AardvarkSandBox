namespace DslFirstImplementation

open Aardvark.Base
open Common
open Common2

[<AutoOpen>]
module ConvertToSceneGraph =

    let viewDrawing (m : DrawingModel) =
        let viewPolygon (p : list<V3d>) =
            [ for edge in Polygon3d(p |> List.toSeq).EdgeLines do
                let v = edge.P1 - edge.P0
                yield Cylinder(edge.P0,v.Normalized,v.Length,0.03) |> render Pick.ignore 
            ] |> group
    
        group [
            yield [ Quad (Quad3d [| V3d(-1,-1,0); V3d(1,-1,0); V3d(1,1,0); V3d(-1,1,0) |]) 
                        |> render [ 
                                on Event.move MoveCursor
                                on Event.leftDown  AddPoint 
                                on Event.rightDown (constF ClosePolygon) // constF == fun a -> fun _ -> a
                            ] 
                    ] |> colored C4b.Gray
            match m.working with
                | Some v when v.cursor.IsSome -> 
                    yield 
                        [ Sphere(V3d.OOO,0.1) |> render Pick.ignore ] 
                            |> colored C4b.Red
                            |> transform' (Trafo3d.Translation(v.cursor.Value))
                    yield viewPolygon (v.cursor.Value :: v.finishedPoints)
                | _ -> ()
            for p in m.finished do yield viewPolygon p
        ]
    
    let drawingApp = { 
        initial = { finished = []; working = None }
        update  = updateDrawing
        view    = viewDrawing
    }

    type State = { trafo : Trafo3d; color : C4b }

    open Aardvark.SceneGraph

    let toSg (scene : Scene<'msg>) : ISg = 
        let rec toSg (s : State) (scene : Scene<'msg>) =
            match scene with
                | Transform(t,children) -> children |> Seq.map ( toSg { s with trafo = s.trafo * t } ) |> Sg.ofSeq
                | Scene.Colored(c,children) -> children |> Seq.map ( toSg { s with color = c } ) |> Sg.ofSeq
                | Render (_, Cone(center,dir,height,radius)) -> 
                    IndexedGeometryPrimitives.solidCone center dir height radius 10 s.color 
                    |> Sg.ofIndexedGeometry |> Sg.transform s.trafo
                | Render (_, Primitive.Cylinder(center,dir,height,radius)) -> 
                    IndexedGeometryPrimitives.solidCylinder center dir height radius radius 10 s.color 
                    |> Sg.ofIndexedGeometry |> Sg.transform s.trafo
                | Render (_, Primitive.Sphere(center,radius)) ->
                    IndexedGeometryPrimitives.solidSubdivisionSphere (Sphere3d(center,radius)) 5 s.color 
                    |> Sg.ofIndexedGeometry |> Sg.transform s.trafo
                | Render(_, Quad(p)) ->
                    IndexedGeometry(
                            IndexedGeometryMode.TriangleList, [| 0; 1; 2; 0; 2; 3 |], 
                            SymDict.ofList [
                                DefaultSemantic.Positions, p.Points |> Seq.map V3f |> Seq.toArray :> System.Array; 
                                DefaultSemantic.Colors,  Array.replicate 4 s.color  :> System.Array; 
                                DefaultSemantic.Normals, Array.replicate 4 (p.Edge03.Cross(p.P2-p.P0)).Normalized :> System.Array
                            ], SymDict.empty)  |> Sg.ofIndexedGeometry |> Sg.transform s.trafo
                | Group xs -> xs |> Seq.map ( toSg s) |> Sg.ofSeq

        toSg { trafo = Trafo3d.Identity; color = C4b.White } scene
