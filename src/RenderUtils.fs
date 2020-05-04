namespace Demo 

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

            
    let createTonemapRenderControl (m : AdaptiveModel) (scene : ISg<'a>) = 

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
                        
                let hdrColorSig = clientValues.runtime.CreateFramebufferSignature(1, [
                        DefaultSemantic.Colors, RenderbufferFormat.Rgba32f; 
                        DefaultSemantic.Depth, RenderbufferFormat.Depth24Stencil8
                        ]
                    )       
                            
                let lumSig = clientValues.runtime.CreateFramebufferSignature(1, [
                        DefaultSemantic.Colors, RenderbufferFormat.R32f; 
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
                let sceneTex = RenderTask.getResult DefaultSemantic.Colors sceneTaskWithClear
                                    
                let lumInitTask = screenQuad 
                                |> Sg.shader { do! ToneMapping.lumInit }
                                |> Sg.uniform "SceneTexture" sceneTex
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

                // NOTE: FSharp.Data.Adaptive equality does no longer propagate outdated marking if value is ReferenceEqual 
                //       -> either overwrite ShallowEqualityComparer or use RenderTask.custom that also performs GenerateMipMaps
                //let temp = RenderTask.renderTo lumFbo lumInitTask
                //let lumTex = temp |> AVal.map (fun x -> 
                //                                let fboOut = x.Attachments.[DefaultSemantic.Colors] :?> BackendTextureOutputView
                //                                let tex = fboOut.texture
                //                                clientValues.runtime.GenerateMipMaps(tex)
                //                                tex 

                let lumTex = RenderTask.custom(fun (self, rt, out) ->
                                                    lumInitTask.Run(rt, out)
                                                    let outColorTex = out.framebuffer.Attachments.[DefaultSemantic.Colors] :?> BackendTextureOutputView
                                                    clientValues.runtime.GenerateMipMaps(outColorTex.texture)
                                                )
                                        |> RenderTask.renderTo lumFbo
                                        |> RenderTask.getResult DefaultSemantic.Colors

                let sgFinal = 
                    screenQuad
                        |> Sg.shader {
                                do! ToneMapping.tonemap
                            }
                        |> Sg.uniform "SceneTexture" sceneTex
                        |> Sg.uniform "LumTexture" lumTex
                        |> Sg.uniform "ExposureMode" m.exposureMode
                        |> Sg.uniform "MiddleGray" m.key
                        |> Sg.uniform "Exposure" m.exposure
                        |> Sg.depthTest (AVal.constant DepthTestMode.None)

                sgFinal
            )

