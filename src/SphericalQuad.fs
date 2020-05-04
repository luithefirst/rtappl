namespace Demo

open Aardvark.Base
open FShade.Imperative

(*
    Spherical Quad for Sampling and Solid Angle Calculation

    Based on Ureña, C., Fajardo, M., & King, A. (2013, July). 
    An Area‐Preserving Parametrization for Spherical Rectangles. 
    In Computer Graphics Forum (Vol. 32, No. 4, pp. 59-66). 
    Blackwell Publishing Ltd.

    https://www.arnoldrenderer.com/research/egsr2013_spherical_rectangle.pdf
*)
module SphericalQuad = 


    (*
        Listing
    *)
    type SphQuad = {
        // local reference system ’R’
        o : V3d
        x : V3d
        y : V3d
        z : V3d

        z0   : float
        z0sq : float

        // rectangle coords in ’R’
        x0   : float
        y0   : float
        y0sq : float

        x1   : float
        y1   : float
        y1sq : float

        // misc precomputed constants
        b0   : float
        b1   : float
        b0sq : float
        k    : float

        // solid angle of ’Q’
        S    : float
    }

    (*
        Listing 2: Precomputation of constants for the spherical rectangle Q
    *)
    [<ReflectedDefinition>] [<Inline>]
    let sphQuadInit (s : V3d) (ex : V3d) (ey : V3d) (o : V3d) = 

        let exl = Vec.length ex
        let eyl = Vec.length ey
        
        // compute local reference system ’R’
        let x = ex / exl
        let y = ey / eyl
        let mutable z = Vec.cross x  y
            
        // compute rectangle coords in local reference system
        let d = s - o
        let mutable z0 = Vec.dot d z

        // flip ’z’ to make it point against ’Q’
        if z0 > 0.0 then
            z <- z * -1.0
            z0 <- z0 * -1.0

        let z = z
        let z0 = z0

        let z0sq = z0 * z0
        let x0 = Vec.dot d x
        let y0 = Vec.dot d y
        let x1 = x0 + exl
        let y1 = y0 + eyl
        let y0sq = y0 * y0
        let y1sq = y1 * y1

        // create vectors to four vertices
        let v00 = V3d(x0, y0, z0)
        let v01 = V3d(x0, y1, z0)
        let v10 = V3d(x1, y0, z0)
        let v11 = V3d(x1, y1, z0)

        // compute normals to edges
        let n0 = (Vec.cross v00 v10) |> Vec.normalize
        let n1 = (Vec.cross v10 v11) |> Vec.normalize
        let n2 = (Vec.cross v11 v01) |> Vec.normalize
        let n3 = (Vec.cross v01 v00) |> Vec.normalize

        // compute internal angles (gamma_i)
        let g0 = (clamp -1.0 1.0 (-Vec.dot n0 n1)) |> acos
        let g1 = (clamp -1.0 1.0 (-Vec.dot n1 n2)) |> acos
        let g2 = (clamp -1.0 1.0 (-Vec.dot n2 n3)) |> acos
        let g3 = (clamp -1.0 1.0 (-Vec.dot n3 n0)) |> acos

        // compute predefined constants
        let b0 = n0.Z
        let b1 = n2.Z
        let b0sq = b0 * b0
        let k = Constant.PiTimesTwo - g2 - g3

        // compute solid angle from internal angles
        let S = g0 + g1 - k

        {
            o = o
            x = x
            y = y
            z = z

            z0   = z0
            z0sq = z0sq

            x0   = x0
            y0   = y0
            y0sq = y0sq

            x1   = x1
            y1   = y1
            y1sq = y1sq

            b0   = b0
            b1   = b1
            b0sq = b0sq
            k    = k

            S    = S
        }
 
    (*
        Listing 3: Sample function using map M(u, v) that returns point p in the planar rectangle P
    *)
    [<ReflectedDefinition>] [<Inline>]
    let sphQuadSample squad u v = 

        // 1. compute 'cu'
        let au = u * squad.S + squad.k
        let fu = ((cos au) * squad.b0 - squad.b1) / (sin au)
        let mutable cu = 1.0 / sqrt(fu*fu + squad.b0sq) * (if fu > 0.0 then 1.0 else -1.0)
        cu <- clamp -1.0 1.0 cu // avoid NaNs

        // 2. compute 'xu'
        let mutable xu = -(cu * squad.z0) / sqrt(1.0 - cu * cu)
        xu <- clamp squad.x0 squad.x1 xu // avoid Infs

        // 3. compute 'yv'
        let d = sqrt(xu * xu + squad.z0sq)
        let h0 = squad.y0 / sqrt(d*d + squad.y0sq)
        let h1 = squad.y1 / sqrt(d*d + squad.y1sq)
        let hv = h0 + v * (h1 - h0)
        let hv2 = hv * hv
        let yv = if (hv2 < 1.0 - 1e-6) then (hv * d) / sqrt(1.0 - hv2) else squad.y1

        // 4. transform (xu, yv, z0) to world coords
        squad.o + xu*squad.x + yv*squad.y + squad.z0*squad.z