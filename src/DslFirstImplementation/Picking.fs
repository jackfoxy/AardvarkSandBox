namespace DslFirstImplementation

open System
open Aardvark.Base

module Picking =
    // implement ray - object intersections for all renderable primitives:
    let hitPrimitive (p : Primitive) (trafo : Trafo3d) (ray : Ray3d) action =
        let mutable ha = RayHit3d.MaxRange
        match p with
            | Sphere(center,radius)-> 
                let transformed = trafo.Forward.TransformPos(center)
                let mutable ha = RayHit3d.MaxRange
                if ray.HitsSphere(transformed,radius,0.0,Double.PositiveInfinity, &ha) then
                    [ha.T, action]
                else []
            | Cone(center,dir,height,radius) | Cylinder(center,dir,height,radius) -> 
                let cylinder = Cylinder3d(trafo.Forward.TransformPos center,trafo.Forward.TransformPos (center+dir*height),radius)
                let mutable ha = RayHit3d.MaxRange
                if ray.Hits(cylinder,0.0,Double.MaxValue,&ha) then
                    [ha.T, action]
                else []
            | Quad q -> 
                let transformed = Quad3d(q.Points |> Seq.map trafo.Forward.TransformPos)
                if ray.HitsPlane(Plane3d.ZPlane,0.0,Double.MaxValue,&ha) then [ha.T, action]
                else []

    // given a ray and a scene -> perform a PickOperation.
    let pick (r : Ray3d) (s : Scene<'msg>)  =
        let rec go (state : State) s = 
            match s with    
                // collect pick operations for all children
                | Group xs -> xs |> Seq.toList |>  List.collect (go state)
                | Transform(t,xs) -> xs |> Seq.toList |> List.collect (go { state with trafo = state.trafo * t })
                | Scene.Colored(_,xs) -> xs |> Seq.toList |> List.collect (go state)
                | Render(action,p) -> 
                    // do the actual work
                    hitPrimitive p state.trafo r action
        match s |> go { trafo = Trafo3d.Identity; color = C4b.White } with
            | [] -> []
            | xs -> 
                // sort PickOperations by camera distance
                xs |> List.filter (not << List.isEmpty << snd) |>  List.sortBy fst 

