namespace Demo

open FShade
open Aardvark.Base.Rendering
open Aardvark.Base
open EffectUtils

module Cubature =

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    } 
    

    type UniformScope with
        member x.PolygonNormal      : V3d  = x?PolygonNormal
        member x.Vertices           : Arr<N<MAX_VERTEXCOUNT>, V3d> = x?Vertices
        member x.VertexCount        : int  = x?VertexCount
    
    type ClosestPointCase = 
        | Inside = 0
        | Edge = 1
        | Vertex = 2
    
    [<ReflectedDefinition>][<Inline>] 
    let clampPointToPolygon_withCase (polygonVertices : Arr<N<MAX_VERTEXCOUNT_PLUS_ONE>, V4d>) (polygonVertexCount : int) (polygonClockwiseOrder : bool) (p : V3d) = 
    
        let mutable case = ClosestPointCase.Inside
        let mutable closestIndex = -1 // inside
        let mutable clampedPoint = p
        let mutable smallestDist = 10000000.0

        let flipSng = if polygonClockwiseOrder then -1.0 else 1.0
            
        let mutable v0 = polygonVertices.[polygonVertexCount-1].XYZ
        for i in 0 .. polygonVertexCount-1 do
                    
            let v1 = polygonVertices.[i].XYZ
    
            let edgePlaneN = (Vec.cross v0 v1)
            let dotPlane = Vec.dot edgePlaneN p
    
            // check if point is outside the polygon
            if flipSng * dotPlane > -1e-9 then
                
                let ab = v1 - v0
                let ap = p - v0
                let lenSq = Vec.dot ab ab
                let t = if lenSq > 1e-5 then
                            (Vec.dot ab ap) / lenSq
                        else 
                            0.0
    
                let projectedPoint = v0 + (clamp 0.0 1.0 t) * ab
          
                // check for projected point distance -> take closest
                let dist = Vec.lengthSquared (projectedPoint - p)
                if dist < smallestDist then
                                
                    clampedPoint <- projectedPoint
                    case <- if t > 0.001 && t < 0.999 then ClosestPointCase.Edge else ClosestPointCase.Vertex
                    closestIndex <- if t > 0.999 then i else (if i > 0 then i - 1 else polygonVertexCount-1)
                    smallestDist <- dist

                    // if point is projected to within the edge -> only possible case of closest point for convex polygons
                    //if case = ClosestPointCase.Edge then
                    //    brk()
    
            v0 <- v1
        
        (clampedPoint, closestIndex, case)
        

    (*
        Computes the solid angle for a planar triangle as seen from the origin.

        Van Oosterom, A., & Strackee, J. (1983). 
        The solid angle of a plane triangle. 
        IEEE transactions on Biomedical Engineering, (2), 125-126.
      
        https://en.wikipedia.org/wiki/Solid_angle#Tetrahedron
    *)
    [<ReflectedDefinition>] [<Inline>]
    let computeSolidAngle_Norm (va : V3d) (vb : V3d) (vc : V3d) =
    
        let numerator = abs (Vec.dot va (Vec.cross vb vc))
        let denom = 1.0 + (Vec.dot va vb) + (Vec.dot va vc) + (Vec.dot vb vc)
        
        let halfSA = atan2 numerator denom

        2.0 * if halfSA >= 0.0 then halfSA else halfSA + Constant.Pi


    let cubature (ltcSpecular : bool) (usePhotometry : bool) (v : Vertex) = 
        fragment {
    
            let mutable P = v.wp.XYZ
            let n = v.n |> Vec.normalize
    
            // create orthonormal basis around N
            let o = (uniform.CameraLocation - P) |> Vec.normalize
            let dotNV = Vec.dot n o
    
            let T1 = (o - (n * dotNV))  |> Vec.normalize
            let T2 = (Vec.cross n T1) |> Vec.normalize
            let w2t = M33d.FromRows(T1, T2, n)
                
            let l = (uniform.Vertices.[0] - P) |> Vec.normalize
            let dotOut = Vec.dot uniform.PolygonNormal l
            let planeEps = 1e-3
            if abs dotOut < planeEps then
                let shiftDist = (planeEps - (abs dotOut)) * l.Length
                let shiftDir = (if dotOut > 0.0 then 1.0 else -1.0) * uniform.PolygonNormal
                let shift = shiftDist * shiftDir
                P <- P + shift
                            
            // clip polygon by tangent plane
            let (clippedVa, clippedVc) = clipPolygonTS4 uniform.Vertices uniform.VertexCount P w2t
            
            let mutable color = V3d.Zero
            if clippedVc > 2 then

                let lightPlaneN = (w2t * uniform.PolygonNormal) |> Vec.normalize
    
                // find closest point limited to upper hemisphere
                let t = Vec.dot clippedVa.[0].XYZ lightPlaneN
                let closestPoint = t * lightPlaneN
                                                       
                // clamp closest point to clipped polygon
                let ccw = if t > 0.0 then true else false
                let (closestPointClamped, i0, pointCase) = clampPointToPolygon_withCase clippedVa clippedVc ccw closestPoint
                let closestPointDir = closestPointClamped |> Vec.normalize
    
                // normalize vectors and init intensities
                for i in 0..clippedVc-1 do
                    let dir = clippedVa.[i].XYZ |> Vec.normalize
                    let iw = -(mulT w2t dir) 
                    let Le = Photometry.getRadiance_World iw usePhotometry // note: includes 1/dotOut
                    set clippedVa.[i] (V4d(dir, Le))
        
                // init triangle count: VertexCount in case of closest point is inside polygon
                //                      VertexCount-1 triangles in case of edge
                //                      VertexCount-2 triangles in case of corner
                let tc = clippedVc - (int)pointCase 
                        
                // init fixed vertex data of triangle fan
                let mutable v0 = Unchecked.defaultof<_>
                if pointCase <> ClosestPointCase.Vertex then                    
                    let dir = closestPointDir
                    let iw = -(mulT w2t dir)
                    let Le = Photometry.getRadiance_World iw usePhotometry // note: includes 1/dotOut
                    v0 <- V4d(closestPointDir, Le)
                else
                    v0 <- clippedVa.[i0]
                        
                let mutable denom = 0.0
                let mutable Ld = 0.0
                    
                for i in 1..tc do
                    let i1 = (i0 + i) % clippedVc
                    let i2 = (i0 + i+1) % clippedVc
                                
                    let sphEx = computeSolidAngle_Norm v0.XYZ clippedVa.[i1].XYZ clippedVa.[i2].XYZ
                              
                    let avgLe = (v0.W + clippedVa.[i1].W + clippedVa.[i2].W) / 3.0
                    let avgG = (v0.Z + clippedVa.[i1].Z + clippedVa.[i2].Z) / 3.0
                    let G = sphEx * avgG
                    Ld <- Ld + avgLe * G
                    denom <- denom + G
                            
                if Ld > 0.0 then 
    
                    // diffuse shading
                    let brdf = v.c.XYZ * Constant.PiInv
                    color <- color + (Ld / 3.0) * brdf
    
                    // specular
                    if ltcSpecular && denom > 0.0 then
                        let Le = Ld / denom
    
                        let ks = V3d.III
                        let roughness = 0.1
                                                                    
                        let ltcSpec = LTC.evalLTCSpec P w2t roughness dotNV uniform.Vertices uniform.VertexCount
                                            
                        color <- color + ks * (ltcSpec * Le)

    
            return V4d(color, v.c.W)
        }