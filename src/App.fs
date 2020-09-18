namespace Demo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Rendering.Text
open Aardvark.Data.Photometry
open FShade
   
module App =
        
    let initLightTransform = Trafo3d.RotationX(Constant.PiHalf) * Trafo3d.Translation(0.0, 0.0, 0.7)
    let initLightPolygon = Polygon2d(V2d(-0.5, -0.5), V2d(0.5, -0.5), V2d(0.5,  0.5), V2d(-0.5,  0.5))
    let translationStepSize = 0.1
    let rotationStepSize = Constant.Pi / 18.0

    let update (m : Model) (msg : Message) =
        match msg with
            
            | SetRenderMode rm -> { m with renderMode = rm }
            | ToggleLTCSpecular -> { m with ltcSpecular = not m.ltcSpecular }
            | ToggleDifferenceRender -> { m with difference = not m.difference }
            | SetSampleCount sc -> { m with refSamplesPerFrame = int sc }
            | SetSamplingMode sm -> { m with refSamplingMode = sm }
            | ToggleAccumulation -> { m with refAccumulation = not m.refAccumulation }

            | ResetLightTransform -> { m with transform = initLightTransform }
            | ChangeLightTransformMode tm -> { m with transformMode = tm }
            | TranslateLight t -> { m with transform = m.transform * Trafo3d.Translation(t) }
            | RotateLight r -> { m with transform = Trafo3d.RotationEuler(r) * m.transform }
            | LoadPhotometry fl -> 
                if fl |> Seq.isEmpty then m
                else
                    let (data, name) = 
                        try
                            let data = LightMeasurementData.FromFile(fl |> Seq.head)
                            let name = data.Name
                            (Some data, Some name)
                        with e -> 
                            Log.warn "%A" e
                            (None, None)
                    { m with photometryData = data; photometryName = name; usePhotometry = true }
            | ResetPhotometry -> { m with photometryData = None; photometryName = None; usePhotometry = false }
            | ToggleUsePhotometry -> { m with usePhotometry = not m.usePhotometry }
            | SetDiffuseExitance d -> { m with diffuseExitance = d }

            | CameraMessage msg ->
                // perform fake update for continuous rendering when in Reference or Difference render mode
                // a dependency to clientValues.time is used, but it is only updated as long as any messages are processed
                let m = 
                    if m.renderMode = RenderMode.Reference || m.difference then 
                        match msg with 
                        | FreeFlyController.Rendered -> 
                            let fakeUpdate = m.cameraState.view.WithLocation(m.cameraState.view.Location)
                            { m with cameraState = { m.cameraState with view = fakeUpdate } }
                        | _ -> m
                    else
                        m
                // forward camera messages
                { m with cameraState = FreeFlyController.update m.cameraState msg }

            | SetExposure v -> { m with exposure = v }
            | SetKey v -> { m with key = v }
            | SetExposureMode em -> { m with exposureMode = em }


            | NOP -> m

    let initial = 
        update {    
            // render settings
            renderMode = RenderMode.Cubature
            difference = false
            ltcSpecular = false

            // light
            transform       = initLightTransform
            transformMode   = Translate
            polygon         = initLightPolygon
            diffuseExitance = 100.0
            usePhotometry   = false
            photometryName  = None
            photometryData  = None

            // ground truth 
            refSamplingMode     = ReferenceSamplingMode.SolidAngle
            refSamplesPerFrame  = 16
            refAccumulation     = true

            // tone-mapping
            exposureMode = ExposureMode.Manual
            exposure = -1.0
            key = 0.12

            cameraState = FreeFlyController.initial 

        } (Message.LoadPhotometry ["..\\..\\..\\photometry\\MIREL_42925637.LDT"])

    let enumValuesToDomNodes<'msg, 'v, 'a when 'a : enum<'v>> (f : 'a -> DomNode<'msg>) =
        let values = Enum.GetValues typeof<'a> :?> ('a [])
        AMap.ofArray(values |> Array.map (fun c -> (c, f c)))

    
     
    let lightUniforms (m : AdaptiveModel) (sceneGraph : ISg<'a>) =

        let sampler = m.photometryData |> AVal.map (Option.map IntensityProfileSampler)

        let lightOrientation = m.transform |> AVal.map (fun t -> t.Backward.UpperLeftM33()) // world to light coordinate transform
        let lightNormal = m.transform |> AVal.map (fun t -> t.Forward.C2.XYZ)

        let polygonVertices = m.polygon |> AVal.map2 (fun (t : Trafo3d) p -> p.GetPointArray (fun (v : V2d) -> V3f(t.Forward.TransformPos(v.XYO)))) m.transform
        let polygonVertexCount = m.polygon |> AVal.map (fun p -> p.PointCount)
        let polygonArea = m.polygon |> AVal.map (fun p -> p.ComputeArea())

        let addressing = sampler |> AVal.map (fun x -> x |> Option.map (fun x -> x.AddressingParameters) |> Option.defaultValue V4f.Zero)
        let offsetScale = sampler |> AVal.map (fun x -> x |> Option.map (fun x -> x.ImageOffsetScale) |> Option.defaultValue V4f.Zero)
        let texture = sampler |> AVal.map (fun x -> x |> Option.map (fun x -> PixTexture2d(PixImageMipMap(x.Image), false) :> ITexture) |> Option.defaultValue (NullTexture() :> ITexture))

        let textureCube = sampler |> AVal.map (fun x -> x |> Option.map (fun sam -> 
                                                                    let cube = sam.GetCubeTexture()
                                                                    let cubeFaces = cube.MipMapArray
                                                                    // Z-up (DirectX) to Y-up (OpenGL)
                                                                    // XN YP XP YN ZP ZN -> XP XN ZN ZP YP YN 
                                                                    //  -> in shader use (X, -Z, Y) to perform lookup
                                                                    //cubeFaces.[0].MipArray.[0].ToPixImage<float32>().ToPixImage(Col.Format.RGB).SaveAsImage("C:\\Debug\\XN.exr") // crashes without Gray to RGB conversion
                                                                    //cubeFaces.[1].MipArray.[0].ToPixImage<float32>().ToPixImage(Col.Format.RGB).SaveAsImage("C:\\Debug\\YP.exr")
                                                                    //cubeFaces.[2].MipArray.[0].ToPixImage<float32>().ToPixImage(Col.Format.RGB).SaveAsImage("C:\\Debug\\XP.exr")
                                                                    //cubeFaces.[3].MipArray.[0].ToPixImage<float32>().ToPixImage(Col.Format.RGB).SaveAsImage("C:\\Debug\\YN.exr")
                                                                    //cubeFaces.[4].MipArray.[0].ToPixImage<float32>().ToPixImage(Col.Format.RGB).SaveAsImage("C:\\Debug\\ZP.exr")
                                                                    //cubeFaces.[5].MipArray.[0].ToPixImage<float32>().ToPixImage(Col.Format.RGB).SaveAsImage("C:\\Debug\\ZN.exr")
                                                                    let cubeFaces = [| cubeFaces.[2]; cubeFaces.[0]; cubeFaces.[5]; cubeFaces.[4]; cubeFaces.[1]; cubeFaces.[3] |]
                                                                    let imgCube = PixImageCube(cubeFaces |> Array.map (fun x -> PixImageMipMap(x.MipArray.[0].ToPixImage())))
                                                                    PixTextureCube(imgCube, true) :> ITexture // wantMipMaps = true -> Future work: use mip bias to smoothen illuminaiton in near-field
                                                                ) 
                                                         |> Option.defaultValue (NullTexture() :> ITexture))
                
        let usePhotometry = AVal.map2 (fun ena data -> ena && Option.isSome data) m.usePhotometry m.photometryData
        let diffuseExitance = AVal.bind2 (fun ena data -> if ena && Option.isNone data then AVal.constant 0.0 else m.diffuseExitance) m.usePhotometry m.photometryData

        sceneGraph 
            |> Sg.uniform "DiffuseExitance" diffuseExitance
            |> Sg.uniform "UsePhotometry" usePhotometry
            |> Sg.uniform "LightBasis" lightOrientation
            |> Sg.uniform "ProfileAddressing" addressing
            |> Sg.uniform "TextureOffsetScale" offsetScale
            |> Sg.uniform "PolygonNormal" lightNormal
            |> Sg.uniform "PolygonArea" polygonArea
            |> Sg.uniform "Vertices" polygonVertices
            |> Sg.uniform "VertexCount" polygonVertexCount
            |> Sg.texture Photometry.IntensityTexture.Symbol texture
            |> Sg.texture Photometry.IntensityCube.Symbol textureCube

    let withApproximationEffect (m : AdaptiveModel) (sceneGraph : ISg<'a>) =

        Sg.dynamic (AVal.map3 (fun spec usePh fx ->  
                    sceneGraph 
                        |> Sg.effect [
                                DefaultSurfaces.trafo |> toEffect
                                //DefaultSurfaces.diffuseTexture |> toEffect
                                match fx with
                                | RenderMode.Cubature -> (Cubature.cubature_opt spec usePh) |> toEffect // always use optimized shader
                                | _ -> (Reference.singlePoint spec usePh) |> toEffect
                            ]
                    ) m.ltcSpecular m.usePhotometry m.renderMode)

    let withReferenceEffect (m : AdaptiveModel) (sceneGraph : ISg<'a>) =
        
        Sg.dynamic (AVal.map3 (fun sampleMethod specular usePhotometry ->
                        sceneGraph 
                            |> Sg.effect [
                                DefaultSurfaces.trafo |> toEffect
                                (Reference.referenceLighting sampleMethod specular usePhotometry) |> toEffect
                            ]
                    ) m.refSamplingMode m.ltcSpecular m.usePhotometry)


    let view (m : AdaptiveModel) =
       
        let render = m.polygon |> AVal.map (fun p -> DrawCallInfo((p.PointCount - 2) * 3) |> Sg.render IndexedGeometryMode.TriangleList)
        let vertices = m.polygon |> AVal.map (fun p -> p.GetPointArray() |> Array.map (fun v -> v.XYO.ToV3f()))
        let indices = m.polygon |> AVal.map (fun p -> Array.init ((p.PointCount - 2) * 3) (fun i -> if i % 3 = 0 then 0 else (i % 3) + i / 3))

        let lightShader (v : Effects.Vertex) = 
                            fragment {
                                
                                let C = uniform.CameraLocation
                                let P = V3d(v.wp)
                                
                                let dir = C - P |> Vec.normalize
                                let i = Photometry.getRadiance_World dir uniform?UsePhotometry

                                // add some intensity to light, so it is not completely dark in direction without emission
                                return V4d(V3d(i + 5.0), 0.0) 
                            }
                            
        let lightSg = Sg.dynamic render
                            |> Sg.vertexAttribute DefaultSemantic.Positions vertices
                            |> Sg.index indices
                            |> Sg.trafo m.transform
                            |> Sg.cullMode (AVal.constant CullMode.None)
                            |> Sg.effect [
                                    DefaultSurfaces.trafo |> toEffect
                                    lightShader |> toEffect
                                ]

        let planeGeometry = IndexedGeometryPrimitives.Box.solidBox (Box3d.FromCenterAndSize(V3d.OOO, V3d(50.0, 50.0, 0.1))) C4b.White
        let groundPlaneSg = Sg.ofIndexedGeometry planeGeometry

        let cubatureSg = Sg.ofList [ groundPlaneSg ]
                            |> withApproximationEffect m

        let referenceSg = Sg.ofList [ groundPlaneSg ]
                            |> withReferenceEffect m

        let cubatureSg = Sg.ofList [lightSg; cubatureSg]
                        |> lightUniforms m
                        |> LTC.setLTCSpecularUniforms

        let referenceSg = Sg.ofList [lightSg; referenceSg]
                        |> lightUniforms m
                        |> LTC.setLTCSpecularUniforms
       
        let renderControl = RenderUtils.createRenderControl m cubatureSg referenceSg
        
        let renderModeValues = enumValuesToDomNodes (fun (rm : RenderMode) -> text (Enum.GetName(typeof<RenderMode>, rm)))
        let exposureModeValues = enumValuesToDomNodes (fun (em : ExposureMode) -> text (Enum.GetName(typeof<ExposureMode>, em)))
        let samplingModeValues = enumValuesToDomNodes (fun (sm : ReferenceSamplingMode) -> text (Enum.GetName(typeof<ReferenceSamplingMode>, sm)))

        require Html.semui (
            body [] [

                renderControl

                // sidebar 
                div [style "position: fixed; width:260pt; margin:0px; border-radius:10px; padding:12px; background:DarkSlateGray; color: white; opacity: 0.9"; 
                    (*clientEvent "onmouseenter" "$('#__ID__').animate({ opacity: 1.0 });";  
                    clientEvent "onmouseleave" "$('#__ID__').animate({ opacity: 0.2 });" *)] [
                   
                    h4 [style "color:white"] [text "Rendering"]                          
                    Html.table [
                        Html.row "Render Mode" [ dropdown1 [ clazz "ui inverted selection dropdown" ] renderModeValues m.renderMode SetRenderMode ]
                        Html.row "Difference" [ simplecheckbox { 
                            attributes [clazz "ui inverted toggle checkbox"; style "" ]
                            state m.difference
                            toggle ToggleDifferenceRender
                        } ]

                        Html.row "LTC Specular" [ simplecheckbox { 
                                attributes [clazz "ui inverted toggle checkbox"; style "" ]
                                state m.ltcSpecular
                                toggle ToggleLTCSpecular
                            } ]

                        Html.row "Sampling Mode" [ dropdown1 [ clazz "ui inverted selection dropdown" ] samplingModeValues m.refSamplingMode SetSamplingMode ]

                        Html.row "Sample Count" [ simplenumeric { attributes [clazz "ui inverted input"]; value (m.refSamplesPerFrame |> AVal.map(fun sc -> float sc)); update SetSampleCount; step 1.0; largeStep 1.0; min 1.0; max 64.0; }]

                        Html.row "Accumulation" [ simplecheckbox { 
                                attributes [clazz "ui inverted toggle checkbox"; style "" ]
                                state m.refAccumulation
                                toggle ToggleAccumulation
                            } ]

                        ]

                    h4 [style "color:white"] [text "Light"]                          
                    Html.table [
                        Html.row "Photometry" [
                            openDialogButton 
                                { OpenDialogConfig.file with allowMultiple = false; title = "Select Photometry" }
                                [ clazz "ui gray button"; style "width:140pt; height:25pt"; onChooseFiles LoadPhotometry; onMouseUp (fun btn crd -> if btn = Aardvark.Application.MouseButtons.Right then ResetPhotometry else NOP) ] 
                                [ Incremental.text (m.photometryName |> AVal.map (fun x -> x |> Option.map (fun x -> x.Substring(0, min x.Length 18)) |> Option.defaultValue "<None>")) ]
                            ]
                        Html.row "UsePhotometry" [ simplecheckbox { 
                                attributes [clazz "ui inverted toggle checkbox"; style "" ]
                                state m.usePhotometry
                                toggle ToggleUsePhotometry
                                //content [ Incremental.text (m.usePhotometry |> Mod.map (fun ena -> if ena then " Photometric" else " Diffuse" ))] 
                            } ]
                        Html.row "DiffuseExitance" [ slider { min = 0.0; max = 1000.0; step = 10.0 } [clazz "ui inverted yellow slider"] m.diffuseExitance SetDiffuseExitance ]
                        ]
                   
                    // light transform
                    div [ clazz "ui buttons"] [
                        button [ clazz "ui button"; onClick (fun () -> ChangeLightTransformMode Translate) ] [ text "Translate" ]
                        div [ clazz "or" ] []
                        button [ clazz "ui button"; onClick (fun () -> ChangeLightTransformMode Rotate) ] [ text "Rotate" ]
                    ]
                    text "  "
                    button [ clazz "ui button" ; onClick (fun () -> ResetLightTransform)] [text "Reset"]

                    Incremental.div (AttributeMap.ofList [clazz "ui icon buttons"]) (
                        alist {
                            let! mode = m.transformMode
                                                    
                            yield   button [clazz "ui button"; onClick (fun () -> 
                                            match mode with 
                                            | Translate -> TranslateLight (V3d(0.0, translationStepSize, 0.0)) 
                                            | Rotate -> RotateLight (V3d(0.0, -rotationStepSize, 0.0)) 
                                        )] [
                                        i [ clazz "arrow left icon"][]
                                    ]
                            yield   button [clazz "ui button"; onClick (fun () -> 
                                            match mode with 
                                            | Translate -> TranslateLight (V3d(-translationStepSize, 0.0, 0.0))
                                            | Rotate -> RotateLight (V3d(-rotationStepSize, 0.0, 0.0)) 
                                        )] [
                                        i [ clazz "arrow down icon"][]
                                    ]
                            yield   button [clazz "ui button"; onClick (fun () -> 
                                            match mode with 
                                            | Translate -> TranslateLight (V3d(translationStepSize, 0.0, 0.0))
                                            | Rotate -> RotateLight (V3d(rotationStepSize, 0.0, 0.0))
                                        )] [
                                        i [ clazz "arrow up icon"][]
                                    ]
                            yield   button [clazz "ui button"; onClick (fun () -> 
                                            match mode with 
                                            | Translate -> TranslateLight (V3d(0.0, -translationStepSize, 0.0))
                                            | Rotate -> RotateLight (V3d(0.0, rotationStepSize, 0.0))
                                        )] [
                                        i [ clazz "arrow right icon"][]
                                    ]
                        }
                    )
                    text " "
                    Incremental.div (AttributeMap.ofList [clazz "ui icon buttons"]) (
                        alist {      
                            let! mode = m.transformMode
                            match mode with
                            | Translate ->
                                yield   button [clazz "ui button"; onClick (fun () -> 
                                            TranslateLight (V3d(0.0, 0.0, -translationStepSize))
                                        )] [
                                            i [ clazz "chevron down icon"][]
                                        ]
                                yield   button [clazz "ui button"; onClick (fun () -> 
                                            TranslateLight (V3d(0.0, 0.0, translationStepSize))
                                        )] [
                                            i [ clazz "chevron up icon"][]
                                        ]
                            | Rotate -> ()
                        })  

                    // tone mapping
                    h4 [style "color:white"] [text "Tonemapping"]
                    Html.table [
                        Html.row "Mode" [ dropdown1 [ clazz "ui inverted selection dropdown" ] exposureModeValues m.exposureMode SetExposureMode ]
                        Html.row "Exposure" [ simplenumeric { attributes [clazz "ui inverted input"]; value m.exposure; update SetExposure; step 0.1; largeStep 1.0; min -20.0; max 10.0; }]
                        Html.row "Middle Gray" [ simplenumeric { attributes [clazz "ui inverted input"]; value m.key; update SetKey; step 0.001; largeStep 0.01; min 0.001; max 1.0; }]
                        ]
                ]

            ]) 

    // in order to provide camera animations, we need to compute a set of  
    // background operations (we call threads). The app maintains (just like each other state)
    // a set of threads which will be executed as long as they exist (no manual subscription stuff required).
    let threads (model : Model) = 
        FreeFlyController.threads model.cameraState |> ThreadPool.map CameraMessage // compute threads for camera controller and map its outputs with our CameraAction

    let app =
        {
            initial = initial
            update = update
            view = view
            threads = threads
            unpersist = Unpersist.instance
        }