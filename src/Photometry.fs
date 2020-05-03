namespace Demo

open FShade.Imperative
open FShade
open Aardvark.Base

module Photometry =

    let IntensityTexture   = TypedSymbol<ITexture>("IntensityTexture")
    let ProfileAddressing  = TypedSymbol<V4d>("ProfileAddressing")
    let TextureOffsetScale = TypedSymbol<V4d>("TextureOffsetScale")

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

    /// get photometry intensity from a normalized direction in photometry coordinate system
    [<ReflectedDefinition>] [<Inline>]
    let getIntensity (v : V3d) =
        // Vertical Texture coords
        let phi = 1.0 - acos(clamp -1.0 1.0 v.Z) * Constant.PiInv // normalize to [0..1]
        let phi = clamp 0.0 1.0 ((phi + uniform.ProfileAddressing.X) * uniform.ProfileAddressing.Y)
       
        // Horizontal Texture coords
        // C0:   atan2( 0  1)  =   0
        // C90:  atan2( 1  0)  =  90
        // C180: atan2( 0 -1)  = 180/-180
        // C270: atan2(-1  0)  = -90
        // normalize [-pi..pi] to [0..1] -> invert vector and add 180° (+0.5)
        let theta = (atan2 -v.Y -v.X) * 0.5 * Constant.PiInv + 0.5
        let theta = 1.0 - abs (1.0 - abs (((theta + uniform.ProfileAddressing.Z) * uniform.ProfileAddressing.W) % 2.0))
       
        let offset = uniform.TextureOffsetScale.XZ  //var Offset = V2d(0.5, 0.5) / (intensityTexture.Size);
        let scale = uniform.TextureOffsetScale.YW   //var Scale = (intensityTexture.Size - V2d.II) / intensityTexture.Size;
        let crd = V2d(phi, theta) * scale + offset
        intensityProfileSampler.SampleLevel(V2d(crd.X, 1.0 - crd.Y), 0.0).X


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
            getIntensity v

    [<ReflectedDefinition>] [<Inline>]
    let getRadiance_World (i : V3d) (usePhotometry : bool) =
           
        if not usePhotometry then
            uniform.DiffuseExitance
        else            
            let v = uniform.LightBasis * i
            let int = getIntensity v
            let dotOut = abs v.Z
            int / (dotOut + 1e-5)
            