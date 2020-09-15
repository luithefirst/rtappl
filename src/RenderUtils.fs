namespace Demo 

open FShade

module RenderUtils =

    open Aardvark.SceneGraph
    open Aardvark.Base
    open Aardvark.UI
    open Aardvark.UI.Primitives
    open Aardvark.Base.Rendering
    open FSharp.Data.Adaptive
    
    // TODO: this should be cleaned in Aardvark.Rendering (there are two copies, one not accessible and one in an unnecessary assembly)
    [<AbstractClass>]
    type OutputMod<'a, 'b>(inputs : list<IOutputMod>) =
        inherit AbstractOutputMod<'b>()

        let mutable handle : Option<'a> = None

        abstract member View : 'a -> 'b
        default x.View a = unbox a
        
        abstract member TryUpdate : AdaptiveToken * 'a -> bool
        default x.TryUpdate(_,_) = false

        abstract member Create : AdaptiveToken -> 'a
        abstract member Destroy : 'a -> unit

        override x.Create() =
            for i in inputs do i.Acquire()

        override x.Destroy() =
            for i in inputs do i.Release()
            match handle with
                | Some h -> 
                    x.Destroy h
                    handle <- None
                | _ ->
                    ()

        override x.Compute(t, rt) =
            let handle = 
                match handle with
                    | Some h ->
                        if not (x.TryUpdate(t, h)) then
                            x.Destroy(h)
                            let h = x.Create(t)
                            handle <- Some h
                            h
                        else
                            h
                    | None ->
                        let h = x.Create t
                        handle <- Some h
                        h
            x.View handle
                    
    module OutputMod =
        let custom (dependent : list<IOutputMod>) (create : AdaptiveToken -> 'a) (tryUpdate : AdaptiveToken -> 'a -> bool) (destroy : 'a -> unit) (view : 'a -> 'b) =
            { new OutputMod<'a, 'b>(dependent) with
                override x.Create t = create t
                override x.TryUpdate(t,h) = tryUpdate t h
                override x.Destroy h = destroy h
                override x.View h = view h
            } :> IOutputMod<_> 
            
        let simple (create : AdaptiveToken -> 'a) (destroy : 'a -> unit) =
            { new OutputMod<'a, 'a>([]) with
                override x.Create t = create t
                override x.Destroy h = destroy h
            } :> IOutputMod<_>


    let createCubatureRenderTexture (m : AdaptiveModel) (clientValues : Aardvark.Service.ClientValues) (scene : ISg<'a>) : IOutputMod<ITexture> =

        let hdrColorSig = clientValues.runtime.CreateFramebufferSignature(1, [
            DefaultSemantic.Colors, RenderbufferFormat.Rgba32f; 
            DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
            ]
        )   

        // NOTE: RenderTask.renderTo clears to [0,0,0,1], but we need to clear with [0,0,0,0] so background is not tone mapped
        let clearTask = clientValues.runtime.CompileClear(hdrColorSig, AVal.constant C4f.Zero, AVal.constant 1.0)
        let sceneTask = scene 
                            |> Sg.viewTrafo clientValues.viewTrafo
                            |> Sg.projTrafo clientValues.projTrafo
                            |> Aardvark.SceneGraph.RuntimeSgExtensions.Sg.compile clientValues.runtime hdrColorSig
                            //|> RenderTask.renderToColor clientValues.size

        let fbo = clientValues.runtime.CreateFramebuffer(hdrColorSig, clientValues.size)
        let sceneTaskWithClear = new RenderTask.SequentialRenderTask([|clearTask; sceneTask|]) |> RenderTask.renderTo fbo

        RenderTask.getResult DefaultSemantic.Colors sceneTaskWithClear

    let createReferenceRenderTexture (m : AdaptiveModel) (clientValues : Aardvark.Service.ClientValues) (scene : ISg<'a>) : IOutputMod<ITexture> =

        let hdrColorSig = clientValues.runtime.CreateFramebufferSignature(1, [
            DefaultSemantic.Colors, RenderbufferFormat.Rgba32f; 
            DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
            ]
        )   

        let mutable inputVersion = 0
        let renderVersion = AVal.custom (fun token -> 
            m.refSamplingMode.GetValue(token) |> ignore
            clientValues.viewTrafo.GetValue(token) |> ignore
            clientValues.projTrafo.GetValue(token) |> ignore
            m.usePhotometry.GetValue(token) |> ignore
            m.diffuseExitance.GetValue(token) |> ignore
            m.photometryData.GetValue(token) |> ignore
            m.polygon.GetValue(token) |> ignore

            inputVersion <- inputVersion + 1
            inputVersion
        )

        let accumulatedSampleCount = AVal.init 0
        let haltonSamples = AVal.init (Array.zeroCreate<V2f> EffectUtils.MAX_SAMPLECOUNT)

        let blendMode = accumulatedSampleCount |> AVal.map (fun sc -> 
                if sc = 0 then
                    BlendMode.None
                else    
                    BlendMode(
                        true, 
                        SourceFactor = BlendFactor.One, 
                        DestinationFactor = BlendFactor.SourceAlpha,
                        Operation = BlendOperation.Add,
                        SourceAlphaFactor = BlendFactor.SourceAlpha,
                        DestinationAlphaFactor = BlendFactor.DestinationAlpha,
                        AlphaOperation = BlendOperation.Add
                    )
            )

        let clearTask = clientValues.runtime.CompileClear(hdrColorSig, AVal.constant C4f.Zero, AVal.constant 1.0)
        let sceneTask = scene 
                            |> Sg.viewTrafo clientValues.viewTrafo
                            |> Sg.projTrafo clientValues.projTrafo
                            |> Sg.blendMode blendMode
                            |> Sg.uniform "HaltonSamples" haltonSamples
                            |> Sg.uniform "SampleCount" m.refSamplesPerFrame
                            |> Sg.uniform "AccumulatedSampleCount" accumulatedSampleCount
                            |> Aardvark.SceneGraph.RuntimeSgExtensions.Sg.compile clientValues.runtime hdrColorSig

        let fbo = clientValues.runtime.CreateFramebuffer(hdrColorSig, clientValues.size)
        let mutable rndSeries = HaltonRandomSeries(2, RandomSystem(123))
        let mutable lastRenderVersion = -1
        let sceneTaskWithClear = RenderTask.custom (fun (self, token, fbo) ->
                    
                // check if we need to clear
                let version = renderVersion.GetValue(self)
                let accum = m.refAccumulation.GetValue(self)
                let clr = lastRenderVersion <> version || not accum
                lastRenderVersion <- version

                if clr then clearTask.Run(self, token, fbo)

                // updates random samples 
                let samCount = m.refSamplesPerFrame.GetValue(self)
                let accumCount = if clr then 0 else accumulatedSampleCount.GetValue(self)

                if clr then // reset halton sequence
                    rndSeries <- HaltonRandomSeries(2, RandomSystem(123))
                    transact(fun() -> accumulatedSampleCount.Value <- 0)

                let nextSamples = Array.init samCount (fun i -> 
                    V2f(rndSeries.UniformDouble(0), rndSeries.UniformDouble(1)))
                    
                transact(fun() -> haltonSamples.Value <- nextSamples)
                                            
                sceneTask.Run(self, token, fbo)

                if accum then
                    transact(fun () -> accumulatedSampleCount.Value <- accumCount + samCount) 
                    // add dependency to time for continuous rendering
                    clientValues.time.GetValue(self) |> ignore
                    Log.line "time: %fms" (clientValues.time.GetValue(self).TotalMilliseconds)
                else 
                    Log.line "no time"
            )

        RenderTask.renderTo fbo sceneTaskWithClear
            |> RenderTask.getResult DefaultSemantic.Colors
      
    let screenQuad : ISg<Message> =
        let drawCall = DrawCallInfo(4)
        let positions = [| V3f(-1,-1,0); V3f(1,-1,0); V3f(-1,1,0); V3f(1,1,0) |]
        let texcoords = [| V2f(0,0); V2f(1,0); V2f(0,1); V2f(1,1) |]

        drawCall
            |> Sg.render IndexedGeometryMode.TriangleStrip 
            |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.constant positions)
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (AVal.constant texcoords)

    let createTonemapSg (m : AdaptiveModel) (clientValues : Aardvark.Service.ClientValues) (intputTexture : aval<ITexture>) = 

        let lumSig = clientValues.runtime.CreateFramebufferSignature(1, [
            DefaultSemantic.Colors, RenderbufferFormat.R32f; 
            ]
        )  
                                    
        let lumInitTask = screenQuad 
                        |> Sg.shader { do! ToneMapping.lumInit }
                        |> Sg.uniform "SceneTexture" intputTexture
                        |> Aardvark.SceneGraph.RuntimeSgExtensions.Sg.compile clientValues.runtime lumSig

        let lumTex =
            OutputMod.custom 
                []
                (fun t -> 
                    let sz = clientValues.size.GetValue t
                    let mipCnt = Fun.Log2Int(float sz.NormMax) // TODO: use int overload of next Aardvark.Base version
                    clientValues.runtime.CreateTexture(sz, TextureFormat.R32f, mipCnt, 1))
                (fun t h -> false)
                (fun h -> clientValues.runtime.DeleteTexture h)
                id

        let lumFbo = 
            OutputMod.custom 
                []
                (fun t -> 
                    let tex = lumTex.GetValue t
                    clientValues.runtime.CreateFramebuffer(lumSig,
                        Map.ofList [
                            DefaultSemantic.Colors, tex.GetOutputView(0, 0)
                        ]))
                (fun t h -> false)
                (fun h -> clientValues.runtime.DeleteFramebuffer h)
                id

        let lumTex = RenderTask.custom(fun (self, rt, out) ->
                                            let foo = clientValues.viewTrafo.GetValue(self) // fake dependecny to trigger rendering when scene changes -> otherwise task will not be run as the scene texture keeps the "same" (handle)
                                            lumInitTask.Run(rt, out)
                                            let outColorTex = out.framebuffer.Attachments.[DefaultSemantic.Colors] :?> BackendTextureOutputView
                                            clientValues.runtime.GenerateMipMaps(outColorTex.texture)
                                        )
                                |> RenderTask.renderTo lumFbo
                                |> RenderTask.getResult DefaultSemantic.Colors

        screenQuad
            |> Sg.shader {
                    do! ToneMapping.tonemap
                }
            |> Sg.uniform "SceneTexture" intputTexture
            |> Sg.uniform "LumTexture" lumTex
            |> Sg.uniform "ExposureMode" m.exposureMode
            |> Sg.uniform "MiddleGray" m.key
            |> Sg.uniform "Exposure" m.exposure
            |> Sg.depthTest (AVal.constant DepthTestMode.None)

    type Vertex = {
        [<TexCoord>] tc : V2d
    }

    let  imageTest =
        sampler2d {
            texture uniform?ImageTest
            filter Filter.MinMagMipPoint
            addressU FShade.WrapMode.Wrap
            addressV FShade.WrapMode.Wrap
        }

    let imageRef =
        sampler2d {
            texture uniform?ImageRef
            filter Filter.MinMagMipPoint
            addressU FShade.WrapMode.Wrap
            addressV FShade.WrapMode.Wrap
        }

    let luminanceVec = V3d(0.2126f, 0.7152f, 0.0722f) // CIE XYZ D65

    let differenceEffect (v : Vertex) =
        fragment {
            let imageTest = imageTest.Sample(v.tc).XYZ
            let imageRef = imageRef.Sample(v.tc).XYZ

            let lumTest = Vec.dot imageTest luminanceVec
            let lumRef = Vec.dot imageRef luminanceVec

            let error = lumTest - lumRef
            let sign = sign error
            let error = abs error

            let upp1 = 0.5
            let b1 = V3d(1.0, 0.5089, 0.34902)      // [255, 130,  89] 
            let d1 = V3d(0.21177, 0.29412, 0.69804) // [ 54,  75, 178] 

            let upp2 = 1.0
            let b2 = V3d(1.0, 0.35686, 0.14902)     // [255,  91,  38] 
            let d2 = V3d(0.07059, 0.18039, 0.69804) // [ 18,  46, 178]

            let (cTrue, cFalse, low, upp) =
                if sign > 0 then
                    if error <= upp1 then // Too bright
                        (V3d(1.0), b1, 0.0, upp1) 
                    else 
                        (b1, b2, upp1, upp2) 
                else 
                    if error <= upp1 then // Too dark
                        (V3d(1.0), d1, 0.0, upp1) 
                    else 
                        (d1, d2, upp1, upp2) 

            let error = (clamp low upp error) - low 

            return V4d((lerp cTrue cFalse (error / (upp - low))), 1.0)
        }

    let createRenderControl (m : AdaptiveModel) (approxSg : ISg<'a>) (referenceSg : ISg<'a>)= 

        let frustum = AVal.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

        let screenQuad =
            let drawCall = DrawCallInfo(4)
            let positions = [| V3f(-1,-1,0); V3f(1,-1,0); V3f(-1,1,0); V3f(1,1,0) |]
            let texcoords = [| V2f(0,0); V2f(1,0); V2f(0,1); V2f(1,1) |]

            drawCall
                |> Sg.render IndexedGeometryMode.TriangleStrip 
                |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.constant positions)
                |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (AVal.constant texcoords)

        FreeFlyController.controlledControlWithClientValues m.cameraState CameraMessage frustum (AttributeMap.ofList [style "position: fixed; left: 0; top: 0; width: 100%; height: 100%"]) RenderControlConfig.standard 
            (fun clientValues -> 
                        
                let approxSceneTex = createCubatureRenderTexture m clientValues approxSg
                let referenceSceneTex = createReferenceRenderTexture m clientValues referenceSg
                           
                let approxSg = createTonemapSg m clientValues approxSceneTex
                let referenceSg = createTonemapSg m clientValues referenceSceneTex

                Sg.dynamic (m.difference |> AVal.map2 (fun rm diff -> 
                                                        if diff then
                                                            let referenceTonemaped = 
                                                                referenceSg
                                                                    |> Sg.compile clientValues.runtime clientValues.signature
                                                                    |> RenderTask.renderToColor clientValues.size

                                                            let approxTonemaped = 
                                                                if rm = RenderMode.Reference then
                                                                    referenceTonemaped
                                                                else
                                                                    approxSg 
                                                                        |> Sg.compile clientValues.runtime clientValues.signature
                                                                        |> RenderTask.renderToColor clientValues.size

                                                            screenQuad
                                                                |> Sg.uniform "ImageTest" (approxTonemaped :> aval<ITexture>)
                                                                |> Sg.uniform "ImageRef" (referenceTonemaped :> aval<ITexture>)
                                                                |> Sg.shader {
                                                                        do! differenceEffect
                                                                    }
                                                        elif rm = RenderMode.Reference then
                                                            referenceSg
                                                        else
                                                            approxSg
                                                    ) m.renderMode)
            )
                         
