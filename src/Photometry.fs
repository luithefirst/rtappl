namespace Demo

open FShade
open Aardvark.Base

module Photometry =

    let IntensityTexture   = TypedSymbol<ITexture> "IntensityTexture"
    let ProfileAddressing  = TypedSymbol<V4d> "ProfileAddressing" 
    let TextureOffsetScale = TypedSymbol<V4d> "TextureOffsetScale"

    /// spherical texture containing measurement data
    /// NOTE: data maps depending on symmetry mode to full/half/quater sphere or single row
    let private intensityProfileSampler = 
           sampler2d {
               texture uniform?IntensityTexture
               filter Filter.MinMagLinear
               addressU WrapMode.Border
               addressV WrapMode.Wrap
               borderColor C4f.Black
           }

    type UniformScope with
        member x.ProfileAddressing  : V4d  = x?ProfileAddressing
        member x.TextureOffsetScale : V4d  = x?TextureOffsetScale 

    /// get photometry intensity from a C and gamma angles
    [<ReflectedDefinition>] [<Inline>]
    let getIntensity (c : float, gamma : float) =

        // Vertical angle: texture u coordinate
        let vert = gamma * Constant.PiInv // normalize to [0..1]
        let u = clamp 0.0 1.0 ((vert + uniform.ProfileAddressing.X) * uniform.ProfileAddressing.Y)

        // Horizontal angle: texture v coordinate
        let horz = c * Constant.PiInv * 0.5 // normalize to [0..1]
        let v = 1.0 - abs (1.0 - abs (((horz + uniform.ProfileAddressing.Z) * uniform.ProfileAddressing.W) % 2.0))

        let offset = uniform.TextureOffsetScale.XZ
        let scale = uniform.TextureOffsetScale.YW
        let crd = V2d(u, v) * scale + offset
        intensityProfileSampler.SampleLevel(V2d(crd.X, 1.0 - crd.Y), 0.0).X

    /// get C and gamma angle from normalized direction vector
    [<ReflectedDefinition>] [<Inline>]
    let toCgamma (v : V3d) =
        // [0,0,-1] = 0°
        // [0,0, 1] = 180°
        let gamma = Constant.Pi - acos(clamp -1.0 1.0 v.Z)

        // C0:   atan2( 0  1)  =   0
        // C90:  atan2( 1  0)  =  90
        // C180: atan2( 0 -1)  = 180/-180
        // C270: atan2(-1  0)  = -90
        // normalize [-pi..pi] to [0..1] -> invert vector and add 180°
        let c = Fun.Atan2(-v.Y, -v.X) + Constant.Pi // atan2: -pi..pi -> 0..2pi

        (c, gamma)

    /// C-gamma coordinates
    /// c = [0, 2pi], gamma = [0, pi]
    [<ReflectedDefinition>] [<Inline>]
    let toDir (c : float, gamma : float) =
        let s = sin gamma
        V3d((cos c) * s, (sin c) * s, - cos gamma)

    /// get photometry intensity from normalized direction vector
    [<ReflectedDefinition>] [<Inline>]
    let getIntensity'(v : V3d) =
        let (c, gamma) = toCgamma v
        getIntensity(c, gamma)


    type UniformScope with
        member x.LightBasis : M33d  = x?LightBasis // expected to be orthonormal basis
        member x.UsePhotometry : bool  = x?UsePhotometry 
        member x.DiffuseExitance : float = x?DiffuseExitance

    /// get photometry intensity from a normalized world direction with light transform and option to 
    [<ReflectedDefinition>] [<Inline>]
    let getIntensity_World (i : V3d) (usePhotometry : bool) =
           
        if not usePhotometry then
            let forward = uniform.LightBasis.R2
            uniform.DiffuseExitance * (abs (Vec.dot i forward))
        else            
            let v = uniform.LightBasis * i
            getIntensity' v

    [<ReflectedDefinition>] [<Inline>]
    let getRadiance_World (i : V3d) (usePhotometry : bool) =
           
        if not usePhotometry then
            uniform.DiffuseExitance
        else            
            let v = uniform.LightBasis * i
            let int = getIntensity' v
            let dotOut = abs v.Z
            int / (dotOut + 1e-5)


    ////////////////////////
    // Cubemap photometry

    let IntensityCube = TypedSymbol<ITexture> "IntensityCube"

    let private intensityCubeSampler = 
            samplerCube {
                texture uniform?IntensityCube
                filter Filter.MinMagMipLinear
            }

    /// get photometry intensity from normalized direction vector
    [<ReflectedDefinition>] [<Inline>]
    let getCubeIntensity(v : V3d) =
        let v = V3d(v.X, -v.Z, v.Y)
        intensityCubeSampler.SampleLevel(v, 0.0).X

    /// get photometry intensity from a normalized world direction with light transform and option to 
    [<ReflectedDefinition>] [<Inline>]
    let getCubeIntensity_World (i : V3d) (usePhotometry : bool) =
           
        if not usePhotometry then
            let forward = uniform.LightBasis.R2
            uniform.DiffuseExitance * (abs (Vec.dot i forward))
        else            
            let v = uniform.LightBasis * i
            getCubeIntensity v

    [<ReflectedDefinition>] [<Inline>]
    let getCubeRadiance_World (i : V3d) (usePhotometry : bool) =
           
        if not usePhotometry then
            uniform.DiffuseExitance
        else            
            let v = uniform.LightBasis * i
            let int = getCubeIntensity v
            let dotOut = abs v.Z
            int / (dotOut + 1e-5)
            