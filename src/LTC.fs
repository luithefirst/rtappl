namespace Demo

open Aardvark.Base
open FShade
open EffectUtils

(*
    Module providing Linearly Transformed Cosines
    https://eheitzresearch.wordpress.com/415-2/
*)
module LTC = 
    open Aardvark.UI

    let LTCAmplitudeTexture  = TypedSymbol<ITexture>("LTCAmplitudeTexture")    
    let LTCMatrixTexture     = TypedSymbol<ITexture>("LTCMatrixTexture")  

    let LTCMatSampler = 
        sampler2d {
            texture uniform?LTCMatrixTexture
            filter Filter.MinMagLinear
        }

    let LTCAmpSampler = 
        sampler2d {
            texture uniform?LTCAmplitudeTexture
            filter Filter.MinMagLinear
        }

 
    [<ReflectedDefinition>] [<Inline>]
    let evalLTCSpec (p : V3d) (w2t : M33d) (roughness : float) (dotNV : float) (vertices : Arr<N<MAX_VERTEXCOUNT>, V3d>) (vertexCount : int) = 
        let LTCTexCoords = V2d(roughness, 1.0 - (2.0 * acos(dotNV) * Constant.PiInv)) // flip y-coord because 0penGL

        let LTCMatrix = 
            let m = LTCMatSampler.Sample(LTCTexCoords)        
            let c1 = V3d(1.0, 0.0, m.Y)
            let c2 = V3d(0.0, m.Z, 0.0)
            let c3 = V3d(m.W, 0.0, m.X)

            M33d.FromCols(c1, c2, c3)

        let LTCMatrix = LTCMatrix * w2t
                                            
        let (clippedVa, clippedVc) = clipPolygonTS4 vertices vertexCount p LTCMatrix

        if clippedVc > 2 then

            // abs -> two sided
            let ffPoly = abs (baumFormFactor4(clippedVa, clippedVc))

            let amplitude = LTCAmpSampler.Sample(LTCTexCoords).X

            amplitude * ffPoly * Constant.PiInv * 0.5

        else
            0.0


    module LUTImporter =

        open System.IO
        open FSharp.Data.Adaptive

        let private importTexFromBinary path (texBinReadProcedure : (BinaryReader -> float32[])) =
    
            let binReader = new BinaryReader(File.OpenRead(path))

            binReader.BaseStream.Position <- int64 0
        
            let data = texBinReadProcedure binReader

            let n = int64 64
            let matPixImage = PixImage<float>.Create(data, Col.Format.RGBA ,n,n)
            let ltcMatTex = PixTexture2d(PixImageMipMap(matPixImage), false)   

            ltcMatTex

        let importMatTex =

            let matTexBinReaderProcedure (binReader : BinaryReader) = 
                [|
                    while binReader.BaseStream.Position < binReader.BaseStream.Length do
                        yield binReader.ReadSingle()
                |]

            let tex = importTexFromBinary ("..\\textures\\ltc_mat.bin") matTexBinReaderProcedure

            AVal.constant (tex :> ITexture)

        let importAmpTex = 

            let ampTexBinReaderProcedure (binReader : BinaryReader) = 
                [|
                    let mutable count = 0
                    while binReader.BaseStream.Position < binReader.BaseStream.Length do
                        yield binReader.ReadSingle()

                        count <- count + 1

                        if count = 2 then
                            yield float32 0.0
                            yield float32 1.0
                            count <- 0
                |]

            let tex = importTexFromBinary ("..\\textures\\ltc_amp.bin") ampTexBinReaderProcedure

            AVal.constant (tex :> ITexture)


    let ltcAmpTex = LUTImporter.importAmpTex
    let ltcMatTex = LUTImporter.importMatTex

    let setLTCSpecularUniforms sg =
        sg
        |> Sg.texture LTCAmplitudeTexture.Symbol ltcAmpTex
        |> Sg.texture LTCMatrixTexture.Symbol ltcMatTex