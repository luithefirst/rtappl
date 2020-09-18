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
        // reference rendering specific uniforms
        member x.HaltonSamples      : Arr<N<MAX_SAMPLECOUNT>, V2d> = x?HaltonSamples
        member x.SampleCount        : int  = x?SampleCount
        member x.AccumulatedSampleCount : int  = x?AccumulatedSampleCount

    (*
        Generates a cosine weighted random direction using the 2 random varables x1 and x2.
        See Global Illuminatin Compendium, Dutré 2003, (35)
        PDF = cos(theta)/PI
    *)
    [<ReflectedDefinition>] [<Inline>]
    let cosineSampleHemisphere u1 u2 = 
        // random sample on disk (x,y) and project to hemisphere (z)
        let r = sqrt u1
        let phi = Constant.PiTimesTwo * u2

        V3d(
            r * (cos phi), 
            r * (sin phi), 
            sqrt (1.0 - u1) // u1 = r^2
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

    [<ReflectedDefinition>] [<Inline>]
    let hitLight_tangent (i : V3d) (vt : Arr<N<MAX_VERTEXCOUNT>, V3d>) : bool =
        let t1 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
        let mutable hitLight = t1 > 1e-8
        if not hitLight then
            let t2 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[2] vt.[3]
            hitLight <- t2 > 1e-8
        hitLight

    let referenceLighting (samplingMethod : ReferenceSamplingMode) (spec : bool) (usePhotometry : bool) (v : Vertex) = 
        fragment {

            let P = v.wp.XYZ
            let n = v.n |> Vec.normalize // world-space normal

            let t2w = n |> EffectUtils.basisFrisvad_rev 
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
        
            for si in 0 .. uniform.SampleCount - 1 do
                   
                let u = jitter + uniform.HaltonSamples.[si]
                let u = u - floor u
                let u1 = u.X
                let u2 = u.Y

                // NOTE: this techqniue has difficuties at the poylgon horizon because the ray intersections becomes unstable 
                //       and dotOut/cos(theta) is almost 0
                if samplingMethod = ReferenceSamplingMode.BRDF then
                                   
                    let i = cosineSampleHemisphere u1 u2

                    if hitLight_tangent i vt then
                        //let samplePDF = dotIn / Pi // cosine hemisphere sampling pdf
                        let invPdf = Constant.Pi // NOTE: dotIn cancelled, Pi will actually also cancel by *brdf

                        let worldI = t2w * -i
                                
                        let Le = Photometry.getRadiance_World worldI usePhotometry // includes divisiion by dotOut

                        let Le = Le / uniform.PolygonArea

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
                        let Le = Photometry.getRadiance_World worldI usePhotometry
                        let Le = Le / uniform.PolygonArea

                        L_d <- L_d + (Le * dotIn) * invPdf

                ()

            // diffuse brdf
            let brdfDiff = v.c.XYZ * Constant.PiInv
            let mutable L = L_d * brdfDiff
            
            if spec then

                let ks = V3d.III
                let roughness = 0.1
                let roughness = roughness * roughness // perceptional linear roughness to NDF parameter

                let eyeDir = (uniform.CameraLocation - P) |> Vec.normalize // -eye vector
                let eyeDir = w2t * eyeDir // tanget-space eye vector
                
                let dotVN = eyeDir.Z

                if roughness = 0.0 then // perfect reflection

                    let i = Vec.reflect V3d.OOI -eyeDir

                    if hitLight_tangent i vt then

                        let worldI = t2w * -i
                        let Le = Photometry.getRadiance_World worldI usePhotometry
                        let Le = Le / uniform.PolygonArea

                        // NOTE: GGX brdf in Cubature+LTC does not include Fresnel
                        let brdfSpec = ks

                        L <- L + Le * brdfSpec * float uniform.SampleCount
                else
                    
                    for si in 0 .. uniform.SampleCount - 1 do
                        let u = jitter + uniform.HaltonSamples.[si]
                        let u = u - floor u
                        let u1 = u.X
                        let u2 = u.Y

                        if samplingMethod = ReferenceSamplingMode.BRDF then
                            // TODO add refernce solution with hemisphere sampling
                            // importance sample GGX distribution according to D (X distribution)
                            //  -> random half-vector direction / micro-facet normal
                            //let h = GGX.importanceSampleGGX roughness u1 u2
                            //let h = GGX.importanceSampleGGX_VNDF roughness u1 u2 e
                            let h = GGX.sampleGGXNdf roughness u1 u2
                            let i = Vec.reflect h -eyeDir
                            let dotIN = i.Z // (Vec.dot i n) in tangent space

                            if dotIN > 1e-5 then

                                if hitLight_tangent i vt then

                                    let worldI = t2w * -i
                        
                                    let Le = Photometry.getRadiance_World worldI usePhotometry // includes divisiion by dotOut
                                    let Le = Le / uniform.PolygonArea

                                    // NOTE: GGX brdf in Cubature+LTC does not include a Fresnel
                                    let G2 = GGX.G2_Smith roughness dotIN dotVN // geometry/shadow-masking term
                                    // as PDF=D -> canceld                                
                                    let brdfSpec = ks * G2 // / (4.0 * dotIN * dotVN) normalization term seems to included in the sampling pdf
                                    // sampleGGXNdf used with: "VdotH * NdotL * 4.f / NdotH" in source // TODO

                                    L <- L + Le * brdfSpec


                        elif samplingMethod = ReferenceSamplingMode.Light || useLight then
                            // generates the samples on the light
                            let samplePoint = vt.[0] + u1 * (vt.[1] - vt.[0]) + u2 * (vt.[3] - vt.[0])
                            let samplePointDistSqrd = Vec.lengthSquared samplePoint

                            let sampleDir = samplePoint |> Vec.normalize
                            let dotIN = sampleDir.Z
                
                            if dotIN > 1e-7 then

                                let worldI = t2w * -sampleDir
                                let I = Photometry.getIntensity_World worldI usePhotometry

                                // PDf of area = 1 -> area to solid angle:
                                //let pdf = samplePointDistSqrd / (Area * dotOut)   |* Area |* dotOut
                                let invPdf = 1.0 / samplePointDistSqrd

                                let h = (eyeDir + sampleDir) |> Vec.normalize // half-vector / micro-facet normal
                                let dotHN = h.Z

                                // NOTE: GGX brdf in Cubature+LTC does not include Fresnel
                                let brdfSpec = ks * GGX.GGX_Correlated roughness dotHN dotIN dotVN
                
                                L <- L + I * brdfSpec * invPdf * dotIN

                        if samplingMethod = ReferenceSamplingMode.SolidAngle && not useLight then
                            
                            // This generates the samples on the projected light
                            let invPdf = squad.S // pdf = 1.0 / squad.S
                            
                            let sampleVec = (SphericalQuad.sphQuadSample squad u1 u2) - P
                            
                            let sampleDir = sampleVec |> Vec.normalize
                            let i = w2t * sampleDir

                            let dotIN = i.Z 
                            if dotIN > 1e-7 then  
                                let Le = Photometry.getRadiance_World -sampleDir usePhotometry
                                let Le = Le / uniform.PolygonArea

                                let h = (eyeDir + sampleDir) |> Vec.normalize // half-vector / micro-facet normal
                                let dotHN = h.Z

                                let brdfSpec = ks * GGX.GGX_Correlated roughness dotHN dotIN dotVN

                                L <- L + Le * brdfSpec * invPdf * dotIN

            

            let newAccumCount = float (uniform.AccumulatedSampleCount + uniform.SampleCount)
            let L = (L / newAccumCount)

            let prevFactor = (float)(uniform.AccumulatedSampleCount) / newAccumCount

            return V4d(L, prevFactor)
        }

    let singlePoint (spec : bool) (usePhotometry : bool) (v : Vertex) =
        fragment {

            let ex = uniform.Vertices.[1] - uniform.Vertices.[0]
            let ey = uniform.Vertices.[3] - uniform.Vertices.[0]

            let lightPos = uniform.Vertices.[0] + (ex + ey) * 0.5

            let lightVec = lightPos - v.wp.XYZ
            let lightDir = (lightPos - v.wp.XYZ) |> Vec.normalize

            let I = Photometry.getIntensity_World -lightDir usePhotometry

            let n = v.n |> Vec.normalize
            let dotIN = max 0.0 (Vec.dot n lightDir)

            let mutable Le = V3d.OOO

            if dotIN > 0.0 then
                let brdfDiff = v.c.XYZ * Constant.PiInv

                let Li = I * dotIN / (lightVec.LengthSquared + 1e-7)
                Le <- brdfDiff * Li

                if spec then
                
                    let ks = V3d.III
                    let roughness = 0.1 // linear roughness

                    let eyeDir = (uniform.CameraLocation - v.wp.XYZ) |> Vec.normalize
                    let dotVN = Vec.dot eyeDir n

                    if dotVN > 0.0 then
                        let h = (eyeDir + lightDir) |> Vec.normalize // half-vector / micro-facet normal
                        let dotHN = Vec.dot h n

                        // NOTE: GGX brdf in Cubature+LTC does not include Fresnel
                        let alpha = roughness * roughness // perceptional linear roughness to NDF parameter
                        let brdfSpec = ks * GGX.GGX_Correlated alpha dotHN dotIN dotVN
                
                        Le <- Le + Li * brdfSpec

            return V4d(Le, 1.0)
        }