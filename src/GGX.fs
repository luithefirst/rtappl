namespace Demo

open Aardvark.Base
open FShade
open EffectUtils
open Aardvark.UI

(*
    GGX BRDF
*)
module GGX =

    /// trowbridge-reitz microfacet distribution in vector form
    /// NDF of the GGX BRDF
    [<ReflectedDefinition>] [<Inline>]
    let GGX_D (m : float) (dotHN : float) = 
        let m2 = m * m
        let f = (dotHN * m2 - dotHN) * dotHN + 1.0
        m2 * Constant.PiInv / (f * f)

    /// GGX Height-Correlated Shadow-Masking
    /// Comparison to Separable: Fig. 7 in "Moving Frostbite to PBR"
    [<ReflectedDefinition>] [<Inline>]
    let G2_Smith (m : float) (dotIN : float) (dotVN : float) =
        let m2 = m * m
        let g1 = dotVN * sqrt (m2 + (1.0 - m2) * dotIN*dotIN)
        let g2 = dotIN * sqrt (m2 + (1.0 - m2) * dotVN*dotVN)
        2.0 * dotIN * dotVN / (g1 + g2)

    [<ReflectedDefinition>] [<Inline>]
    let GGX_Correlated (roughness : float) (dotHN : float) (dotIN : float) (dotVN : float) =
        let D = GGX_D roughness dotHN
        let G2 = G2_Smith roughness dotIN dotVN
        D * G2 / (4.0 * dotIN * dotVN)

    /// https://schuttejoe.github.io/post/ggximportancesamplingpart1/
    /// NOTE: strange "ringing" noise pattern
    [<ReflectedDefinition>] [<Inline>]
    let importanceSampleGGX m u1 u2 =
        let m2 = m*m
        let phi = Constant.PiTimesTwo * u2
        let theta = acos ( sqrt ((1.0 - u1) / (u1 * (m2 - 1.0) + 1.0)) )

        let sinT = sin theta
        let cosT = cos theta
        //let pdf = m2 * cosT * sinT * Constant.PiInv / ((m2 - 1.0) * (cos (theta + 1.0)).Square() + 1.0).Square()
        // only "very similar" to the NDF itself -> TODO canceled remainder
        // -> dotHV / (dotIN * dotON) ??

        // NOTE: Z-up instead of Y-up
        let x = sinT * cos phi
        let y = sinT * sin phi
        let z = cosT
        V3d(x, y, z)


    /// https://github.com/NVIDIA/Q2RTX/issues/40
    /// Visible normal sampling method of: http://jcgt.org/published/0007/04/01/paper.pdf
    /// TODO/NOTE: does not work
    [<ReflectedDefinition>] [<Inline>]
    let importanceSampleGGX_VNDF (m : float) (u1 : float) (u2 : float) (Ve : V3d) =
        let m2 = m * m
        let Vh = V3d(m2 * Ve.X, m2 * Ve.Y, Ve.Z).Normalized

        let lensq = Vh.X.Square() + Vh.Y.Square()
        let T1 = if lensq > 0.0 then V3d(-Vh.Y, Vh.X, 0.0) / (sqrt lensq) else V3d.IOO
        let T2 = Vec.cross Vh T1
    
        let r = sqrt(u1)
        let phi = Constant.PiTimesTwo * u2
        let t1 = r * cos(phi)
        let t2 = r * sin(phi)
        let s = 0.5 * (1.0 + Vh.Z)
        let t2 = (1.0 - s) * sqrt(1.0 - t1.Square()) + s * t2
    
        // Tangent space H
        let Nh = t1 * T1 + t2 * T2 + sqrt(max 0.0 (1.0 - t1.Square() - t2.Square())) * Vh
    
        V3d(m2 * Nh.X, max 0.0 Nh.Z, m2 * Nh.Y)

    /// https://github.com/DQLin/RealTimeStochasticLightcuts/blob/master/RealTimeStochasticLightcuts/Shaders/RayTracing/BRDF.hlsli
    /// NOTE: does not use acos in difference to importanceSampleGGX, but noise pattern also looks different ?
    [<ReflectedDefinition>] [<Inline>]
    let sampleGGXNdf (ggxAlpha : float) (u1 : float) (u2 : float) = 
        let a2 = ggxAlpha * ggxAlpha
        let phi = Constant.PiTimesTwo * u1
        let cosTheta2 = min 1.0 ((1.0 - u2) / (1.0 + (a2 - 1.0) * u2))
        let cosTheta = sqrt cosTheta2
        let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        
        // in tangent frame
        let x = sinTheta * cos phi
        let y = sinTheta * sin phi
        let z = cosTheta
        
        V3d(x, y, z)