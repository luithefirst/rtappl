namespace Demo

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade

module ToneMapping = 

    let ExposureMode = TypedSymbol<ExposureMode>("ExposureMode")
    let Exposure     = TypedSymbol<float>("Exposure")
    let MiddleGray   = TypedSymbol<float>("MiddleGray") 

    type UniformScope with
        member x.ExposureMode : ExposureMode = uniform?ExposureMode
        member x.Exposure : float = x?Exposure
        member x.MiddleGray : float = x?MiddleGray

    let private blitSampler =
        sampler2d {
            texture uniform?BlitTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
    
    type VertexFSQ = {
        [<TexCoord>] tc : V2d
    }

    let blit (v : VertexFSQ) =
        fragment {
            return blitSampler.Sample(v.tc)
        }

    let private sceneTexture =
        sampler2d {
            texture uniform?SceneTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private lumTexture =
        sampler2d {
            texture uniform?LumTexture
            filter Filter.MinMagMipPoint
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let lumVector = V3d(0.2126, 0.7152, 0.0722)

    let lumInit (v : VertexFSQ) =
        fragment {
            let scene = sceneTexture.Sample(v.tc)
            let lum = Vec.dot scene.XYZ lumVector
            let logLumClamped = clamp -10.0 20.0 (log lum)
            return V4d(logLumClamped, 0.0, 0.0, 0.0) 
        }

    [<GLSLIntrinsic("lessThanEqual({0},{1})")>]
    let LessThanEqual<'a when 'a :> IVector> (a : 'a) (b : 'a) : 'a = onlyInShaderCode ""

    [<ReflectedDefinition>] [<Inline>]
    let private LinearToGammaSRGBVec(c : V3d) : V3d =
        let rTrue = c * 12.92
        let rFalse = 1.055 * (pow c (V3d (1.0 / 2.4))) - 0.055

        lerp rFalse rTrue (LessThanEqual c (V3d 0.0031308))

    let tonemap (v : VertexFSQ) =
        fragment {
            let scene = sceneTexture.Sample(v.tc).XYZ
        
            let ev = 
                if uniform.ExposureMode = Demo.ExposureMode.Manual then
                    exp uniform.Exposure
                else
                    let last = lumTexture.MipMapLevels - 1
                    let avgLum = exp (lumTexture.Read(V2i(0, 0), last).X)
                    let key = if uniform.ExposureMode = Demo.ExposureMode.Auto then
                                1.001 - (2.0 / (2.0 + log(avgLum + 1.0) / log(10.0)))
                              else // ExposureMode.MiddleGray
                                uniform.MiddleGray
                    key / avgLum

            let color = scene * ev

            let color = color / (1.0 + color)

            let color = LinearToGammaSRGBVec color

            return V4d(color, 1.0)
        }