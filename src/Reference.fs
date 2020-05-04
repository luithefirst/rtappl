namespace Demo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade
open EffectUtils

(*
    Refernece rendering of photometric polygonal lights
    NOTE: polygon assume to be quad
*)
module Reference =

    type Vertex = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<FragCoord>]       fc      : V4d
    }  

    type UniformScope with
        // light
        member x.PolygonArea        : float  = x?PolygonArea
        member x.PolygonNormal      : V3d  = x?PolygonNormal
        member x.Vertices           : Arr<N<MAX_VERTEXCOUNT>, V3d> = x?Vertices
        member x.VertexCount        : int  = x?VertexCount
        // sampling
        member x.HaltonSamples      : Arr<N<MAX_SAMPLECOUNT>, V2d> = x?HaltonSamples
        member x.SampleCount        : int  = x?SampleCount
        member x.AccumulatedSampleCount : int  = x?AccumulatedSampleCount

    (*
        Samples a direction from the hemisphere for two given random numbers where the samples are cosine weighted
        Works in the tangent space
        http://www.rorydriscoll.com/2009/01/07/better-sampling/
    *)
    [<ReflectedDefinition>] [<Inline>]
    let cosineSampleHemisphere u1 u2 = 
        let r = sqrt u1
        let theta = Constant.PiTimesTwo * u2

        V3d(
            r * (cos theta), 
            r * (sin theta), 
            sqrt (1.0 - u1)
        )

    (*
        Computes the intersaction point of a ray and triangle. 

        Möller, T., & Trumbore, B. (2005, July). 
        Fast, minimum storage ray/triangle intersection.
        In ACM SIGGRAPH 2005 Courses (p. 7). ACM.

        https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
    *)
    [<ReflectedDefinition>] [<Inline>]
    let rayTriangleIntersaction (orig : V3d) (dir : V3d) (v0 : V3d) (v1 : V3d) (v2 : V3d) = 
        let e1 = v1 - v0
        let e2 = v2 - v0
        
        let pVec = Vec.cross dir e2
        let det  = Vec.dot e1 pVec
        
        if (det < 1e-8 && det > -1e-8) then
            0.0
        else
            let invDet = 1.0 / det
            let tVec   = orig - v0
            let u      = (Vec.dot tVec pVec) * invDet

            if (u < 0.0 || 1.0 < u) then
                0.0
            else
                let qVec = Vec.cross tVec e1
                let v    = (Vec.dot dir qVec) * invDet

                if (v < 0.0 || 1.0 < u + v) then
                    0.0
                else 
                    ((Vec.dot e2 qVec) * invDet)

    let referenceLighting (samplingMethod : ReferenceSamplingMode) (usePhotometry : bool) (v : Vertex) = 
        fragment {

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> EffectUtils.basisFrisvad_rev 
            let w2t = t2w |> Mat.transpose
            
            // Compute a jitter
            let jitter = (EffectUtils.fast32Hash v.fc.XYZ).XY

            let mutable L_d = 0.0

            // intialize polygon in tangent space
            let mutable vt = Arr<N<MAX_VERTEXCOUNT>, V3d>() 
            for vi in 0 .. uniform.VertexCount - 1 do
                vt.[vi] <- w2t * (uniform.Vertices.[vi] - P)

            // initialize spherical quad / polygon assumed to be quad
            let ex = uniform.Vertices.[1] - uniform.Vertices.[0]
            let ey = uniform.Vertices.[3] - uniform.Vertices.[0]
            let squad = SphericalQuad.sphQuadInit uniform.Vertices.[0] ex ey P
            // fallback to light sampling if solid angle is below threshold
            let useLight = samplingMethod = ReferenceSamplingMode.SolidAngle && squad.S < 1e-3 // NOTE: stability breaks down at 1e-4
        
            let brdf = v.c.XYZ * Constant.PiInv

            for si in 0 .. uniform.SampleCount - 1 do
                   
                let u = jitter + uniform.HaltonSamples.[si]
                let u = u - floor u
                let u1 = u.X
                let u2 = u.Y

                if samplingMethod = ReferenceSamplingMode.BRDF then
                                
                    //let i = sampleHemisphere u1 u2                
                    //let brdfPDF = 1.0 / PI // uniform spherical sampling

                    let i = cosineSampleHemisphere u1 u2

                    let t1 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                    let mutable hitLight = t1 > 1e-8
                    if not hitLight then
                        let t2 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[2] vt.[3]
                        hitLight <- t2 > 1e-8

                    if hitLight then
                        //let dotIn = i.Z
                        //let brdfPDF = dotIn / Constant.PiTimesTwo // cosine spherical sampling
                        let invPdf = Constant.PiTimesTwo // NOTE: dotIn cancelled

                        let worldI = t2w * -i
                        let dotOut = max 1e-9 (abs (Vec.dot worldI uniform.PolygonNormal))
                                
                        let I = Photometry.getRadiance_World worldI usePhotometry // includes divisiion by dotOut
                        let Le = I / uniform.PolygonArea

                        L_d <- L_d + Le * invPdf

                if samplingMethod = ReferenceSamplingMode.Light || useLight then
                                    
                    // generates the samples on the light
                    let samplePoint = vt.[0] + u1 * (vt.[1] - vt.[0]) + u2 * (vt.[3] - vt.[0])
                    let samplePointDistSqrd = Vec.lengthSquared samplePoint

                    let sampleDir = samplePoint |> Vec.normalize
                    let dotIn = sampleDir.Z
                
                    if dotIn > 1e-7 then

                        let worldI = t2w * -sampleDir
                        let I = Photometry.getIntensity_World worldI usePhotometry

                        // PDf of area = 1 -> area to solid angle:
                        //let pdf = samplePointDistSqrd / (Area * dotOut)   |* Area |* dotOut
                        let invPdf = 1.0 / samplePointDistSqrd
                                                            
                        //let Le = I / (Area * dotOut)    |* Area |* dotOut
                        //let Le = I

                        L_d <- L_d + I * dotIn * invPdf
                            
                if samplingMethod = ReferenceSamplingMode.SolidAngle && not useLight then
                                    
                    // This generates the samples on the projected light
                    let invPdf = squad.S // pdf = 1.0 / squad.S
                                    
                    let samplePoint = (SphericalQuad.sphQuadSample squad u1 u2) - P
                            
                    let worldI = -samplePoint |> Vec.normalize

                    let dotIn = -(w2t * worldI).Z 
                    if dotIn > 1e-7 then  
                        let dotOut = max 1e-9 (abs (Vec.dot worldI uniform.PolygonNormal))

                        let I = Photometry.getRadiance_World worldI usePhotometry
                        let Le = I / uniform.PolygonArea

                        L_d <- L_d + (Le * dotIn) * invPdf

                ()

            let newAccumCount = float (uniform.AccumulatedSampleCount + uniform.SampleCount)
            let L_d = brdf * (L_d / newAccumCount)
            
            let prevFactor = (float)(uniform.AccumulatedSampleCount) / newAccumCount

            return V4d(L_d, prevFactor)
        }