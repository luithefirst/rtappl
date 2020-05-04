namespace Demo

open FShade
open Aardvark.Base.Rendering
open Aardvark.Base

module EffectUtils =

    [<Literal>]
    let MAX_VERTEXCOUNT = 4

    [<Literal>]
    let MAX_VERTEXCOUNT_PLUS_ONE = 5

    [<Literal>]
    let MAX_SAMPLECOUNT = 64


    [<GLSLIntrinsic("{0} = {1}")>] [<KeepCall>]
    let set (a: 'a) (b : 'a) = onlyInShaderCode "set" 

    [<GLSLIntrinsic("break;")>] [<KeepCall>]
    let brk() = onlyInShaderCode "break"

    [<GLSLIntrinsic("mix({0},{1},{2})")>]
    let mix<'a when 'a :> IVector> (a : 'a) (b : 'a) (s : float) : 'a = onlyInShaderCode "mix"

    [<GLSLIntrinsic("({0} * {1})")>]
    let mulT (m : 'a) (v : 'b) : 'b = onlyInShaderCode "mul"
        
    [<GLSLIntrinsic("lessThanEqual({0},{1})")>]
    let LessThanEqual<'a when 'a :> IVector> (a : 'a) (b : 'a) : 'a = onlyInShaderCode ""


    [<ReflectedDefinition>][<Inline>] 
    let private integrateSegment(a: V3d, b: V3d) =              
        let theta = acos (clamp -0.99999 0.99999 (Vec.Dot(a, b)))
        Vec.Cross(a, b).Z * if theta < 1e-5 then 1.0 else theta/sin(theta)

    [<ReflectedDefinition>] [<Inline>]
    let baumFormFactor4(va : Arr<N<MAX_VERTEXCOUNT_PLUS_ONE>, V4d>, vc : int) =
    
        let cpa0 = va.[0].XYZ |> Vec.normalize
        let mutable cpa = cpa0
        let mutable ff = 0.0
    
        for vi in 1..vc-1 do
            let cpaNext = va.[vi].XYZ |> Vec.normalize
    
            ff <- ff + integrateSegment(cpa, cpaNext)
            cpa <- cpaNext
    
        // final segment
        ff <- ff + integrateSegment(cpa, cpa0)
    
        abs (ff * 0.5) // / area

    [<ReflectedDefinition>][<Inline>]
    let clipPolygonTS4 (vertices : Arr<N<MAX_VERTEXCOUNT>, V3d>) (vertexCount : int) (p : V3d) (w2t : M33d) =
        let eps = 1e-9

        let mutable vc = 0
        let va = Arr<N<MAX_VERTEXCOUNT_PLUS_ONE>, V4d>()

        let vb = w2t * (vertices.[0].XYZ - p)
        let hb = vb.Z
        let hbv = hb > eps
        let hbn = hb < -eps
        
        if (hb >= -eps) then 
            set va.[vc].XYZ vb
            vc <- vc + 1

        let mutable v0 = vb
        let mutable h0 = hb
        let mutable h0v = hbv
        let mutable h0n = hbn
        
        for vi in 1..vertexCount-1 do
            let v1 = w2t * (vertices.[vi] - p)
            let h1 = v1.Z
            let h1v = h1 > eps
            let h1n = h1 < -eps
            if (h0v && h1n || h0n && h1v) then
                set va.[vc].XYZ (mix v0 v1 (h0 / (h0 - h1)))
                vc <- vc + 1

            if (h1 >= -eps) then 
                set va.[vc].XYZ v1
                vc <- vc + 1

            v0 <- v1
            h0 <- h1
            h0v <- h1v
            h0n <- h1n
        
        // last edge to vertices[0]
        if (h0v && hbn || h0n && hbv) then
            set va.[vc].XYZ (mix v0.XYZ vb.XYZ (h0 / (h0 - hb)))
            vc <- vc + 1

        (va,vc)

    (*
        Creates a hash usable as jitter computed from a 2D coordinate
        Taken from https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
    *)
    [<ReflectedDefinition>]
    let fast32Hash (coordinate : V3d) : V4d = 
        let offset = V2d(26.0, 161.0)
        let domain = 71.0
        let someLargeFloat = 951.135664 //+ coordinate.Z

        let mutable P = V4d(coordinate.X, coordinate.Y, coordinate.X, coordinate.Y)
        P <- P + V4d(0.0, 0.0, 1.0, 1.0)
        
        P <- P - floor (P / domain) * domain
        
        P <- P + V4d(offset.X, offset.Y, offset.X, offset.Y)
        P <- P * P

        let xzxz = V4d(P.X, P.Z, P.X, P.Z)
        let yyww = V4d(P.Y, P.Y, P.W, P.W)

        Fun.Frac(xzxz * yyww * V4d(1.0 / someLargeFloat))

    (*
        Building an Orthonormal Basis, Revisited
        branchless version
    *)
    [<ReflectedDefinition>] [<Inline>]
    let basisFrisvad_rev (n : V3d) = 
        //let sg = float (sign n.Z) // produces garbage because sign of 0 is 0 (original: copysignf(1.0f, n.z))
        let sg = if n.Z >= 0.0 then 1.0 else -1.0 
        let a = -1.0 / (sg + n.Z)
        let b = n.X * n.Y * a
        let c1 = V3d(1.0 + sg * n.X * n.X * a, sg * b, -sg * n.X)
        let c2 = V3d(b, sg + n.Y * n.Y * a, -n.Y)

        M33d.FromCols(c1, c2, n)