namespace DslFirstImplementation

open Aardvark.Base
open Aardvark.Application

type Polygon = list<V3d>

type OpenPolygon = {
    finishedPoints  : list<V3d> // points already added to the polygon
    cursor         : Option<V3d> (* the last point of the polygon which can still be modified. 
            Note that the last point of the polygon might be undefined (represented as None). *)
}

type DrawingModel = {
    finished : list<Polygon>
    working  : Option<OpenPolygon>
}

type Primitive = 
    | Sphere      of center : V3d * radius : float
    | Cone        of center : V3d * dir : V3d * height : float * radius : float
    | Cylinder    of center : V3d * dir : V3d * height : float * radius : float
    | Quad        of Quad3d 

type Scene1 = 
    | Transform of Trafo3d * list<Scene1>
    | Colored   of C4b     * list<Scene1>
    | Render    of Primitive 
    | Group     of list<Scene1>

type DrawCommand =
    | ClosePolygon
    | AddPoint   of V3d
    | MoveCursor of V3d

type MouseEvent = 
    | Move of V3d 
    | Down of MouseButtons * V3d

type PickOperation<'msg> = MouseEvent -> Option<'msg>

module Common =
    let scene =
        Transform ( Trafo3d.Translation(10.0,10.0,10.0), [
                      Render ( Sphere(V3d.OOO, 1.0))
                      Render ( Sphere(V3d.OOO, 0.1))
                   ])


    // curried version of Colored in order to nicely compose
    let colored1 c xs = Colored(c,xs)
    let transformed1 t x = Transform(t,[x])

    let view1 (m : DrawingModel) : Scene1 =

        let groundPlane =
            [ Quad (Quad3d [| V3d(-1,-1,0); V3d(1,-1,0); V3d(1,1,0); V3d(-1,1,0) |]) |> Render ] 
            |> colored1 C4b.Gray

        let viewPolygon (p : list<V3d>) : Scene1 = 
            [ for edge in Polygon3d(p |> List.toSeq).EdgeLines do
                let v = edge.P1 - edge.P0
                yield Cylinder(edge.P0,v.Normalized,v.Length,0.03) |> Render
            ] |> Group

        let openPolygon =   
            match m.working with
                | None   -> [] // no open polygon -> just return empty list)
                | Some v -> [viewPolygon v.finishedPoints]

        let cursor =
            match m.working with
                | Some v when Option.isSome v.cursor -> 
                    // if we have a last point (cursor)
                    [ [ Sphere(V3d.OOO,0.1) |> Render ] 
                       |> colored1 C4b.Red
                       |> transformed1 (Trafo3d.Translation(v.cursor.Value))
                    ]
                | _ -> [] // no working polygon or no cursor

        let polygons = 
            m.finished |> List.map viewPolygon

        Group [
            yield  groundPlane
            yield! openPolygon
            yield! cursor
            yield! polygons
            ]

    let updateDrawing (m : DrawingModel) (cmd : DrawCommand) =
        match cmd with
            | AddPoint p -> // we want to add a point to our drawing model
                match m.working with // do we have a polygon we could add to point to?
                    | Some v -> // yes we have, create a new OpenPolygon with our new point added and update the model
                        { m with working = Some { v with finishedPoints = p :: v.finishedPoints }}
                    | None -> { m with working = Some { finishedPoints = [ p ]; cursor = None;  }}
            | ClosePolygon -> 
                match m.working with
                    | None -> m // we have no polygon to close -> identity. the updated model is the old model
                    | Some p -> 
                        { m with 
                            working = None // close a polygon. there is no more open polygon ...
                            finished = p.finishedPoints :: m.finished // ... and the formerly opened polygon is now part of the finished polygons
                        }
            | MoveCursor p -> // the last point of the current working polygon follows the mouse cursor
                match m.working with // check if we have a working polygon
                    | None -> 
                        // start a new open polygon with our current cursor set to mouse position
                        { m with working = Some { finishedPoints = []; cursor = Some p }}
                    | Some v -> 
                        // override current cursor position
                        { m with working = Some { v with cursor = Some p }}

    let groundPlane =
        [ Quad (Quad3d [| V3d(-1,-1,0); V3d(1,-1,0); V3d(1,1,0); V3d(-1,1,0) |]) |> Render ] 
        |> colored1 C4b.Gray

// scene produces messages of type 'msg
type Scene<'msg> = 
        | Transform of Trafo3d * seq<Scene<'msg>>
        | Colored   of C4b     * seq<Scene<'msg>>
        | Render    of list<PickOperation<'msg>> * Primitive // new bits in here
        | Group     of seq<Scene<'msg>>

module Event =
    // "function" is the combined lambda/match syntax
    let move : MouseEvent -> bool = function Move _ -> true | _ -> false
    let down = function Down _ -> true | _ -> false       
    let down' p = function Down(p',_) when p = p' -> true | _ -> false 
    let leftDown = down' MouseButtons.Left
    let rightDown = down' MouseButtons.Right
    let position = function Move s -> s | Down(_, s) -> s

module Common2 =
    let transform t xs = Transform(t,xs)
    let transform' t x = Transform(t,[x])
    let translate x y z xs = Transform(Trafo3d.Translation(x,y,z),xs)
    let colored c xs = Colored(c,xs)
    let render picks primitive = Render(picks,primitive)
    let group xs = Group xs

    let drawGroundPlane =
        Quad (Quad3d [| V3d(-1,-1,0); V3d(1,-1,0); V3d(1,1,0); V3d(-1,1,0) |]) 
            |> render [ 
                    // Pick operations for moving the cursor.
                    // if the ground plane is hit by a mouse move event, construct the MoveCursor command
                    (fun evt -> 
                        match evt with
                            | Move p -> Some (MoveCursor p)
                            | _ -> None
                    )
                    (fun evt -> 
                        match evt with
                            | Down(MouseButtons.Left,p) -> Some (AddPoint p)
                            | _ -> None
                    )
                    (fun evt -> 
                        match evt with
                            | Down(MouseButtons.Right,_) -> Some ClosePolygon
                            | _ -> None)
            ]

    let on (p : MouseEvent -> bool) (r : V3d -> 'msg) (k : MouseEvent) = if p k then Some (r (Event.position k)) else None

    let drawGroundPlane2 =
        Quad (Quad3d [| V3d(-1,-1,0); V3d(1,-1,0); V3d(1,1,0); V3d(-1,1,0) |]) 
            |> render [ 
                    on Event.move      (fun p -> MoveCursor p)            
                    on Event.leftDown  (fun p -> AddPoint p)
                    on Event.rightDown (fun _ -> ClosePolygon) 
               ]

    let drawGroundPlane3 =
        Quad (Quad3d [| V3d(-1,-1,0); V3d(1,-1,0); V3d(1,1,0); V3d(-1,1,0) |]) 
            |> render [ 
                    on Event.move      MoveCursor            
                    on Event.leftDown  AddPoint 
                    on Event.rightDown (fun _ -> ClosePolygon) 
               ]

type App<'model,'msg,'view> =
    {
        initial   : 'model
        update    : 'model   -> 'msg -> 'model
        view      : 'model   -> 'view
    }

type ThreeDApp<'model,'msg> = App<'model,'msg,Scene<'msg>>

module Pick =
    let ignore = [] // empty list of pick operations shorthand
