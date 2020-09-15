namespace Demo

open System
open Aardvark.Base
open Aardvark.UI.Primitives
open Aardvark.Data.Photometry
open Adaptify

type ExposureMode = Manual=0 | MiddleGray=1 | Auto=2
    
type RenderMode =
    | Point = 0
    | Cubature = 1
    //| CubatureOpt = 2
    | Reference = 2
    
type ReferenceSamplingMode =
    | BRDF = 0
    | Light = 1
    | SolidAngle = 2

type LightTransformMode =
    | Translate
    | Rotate

[<ModelType>]
type Model =
    {
        // render settings
        renderMode      : RenderMode
        difference      : bool
        ltcSpecular     : bool

        // light
        transform           : Trafo3d
        transformMode       : LightTransformMode
        polygon             : Polygon2d
        diffuseExitance     : float
        usePhotometry       : bool
        photometryName      : Option<string>
        photometryData      : Option<LightMeasurementData>

        // ground truth 
        refSamplingMode       : ReferenceSamplingMode
        refSamplesPerFrame    : int
        refAccumulation       : bool
        
        // tonemapping
        exposureMode    : ExposureMode
        exposure        : float
        key             : float

        cameraState     : CameraControllerState
    }



type Message =
    
    // rendering settings
    | SetRenderMode of RenderMode
    | ToggleLTCSpecular
    | ToggleDifferenceRender
    | ToggleAccumulation
    | SetSamplingMode of ReferenceSamplingMode
    | SetSampleCount of float

    // light settings
    | LoadPhotometry of list<string>
    | ResetPhotometry
    | ToggleUsePhotometry 
    | SetDiffuseExitance of float

    // light transform
    | ChangeLightTransformMode of LightTransformMode
    | ResetLightTransform
    | TranslateLight of V3d
    | RotateLight of V3d

    | CameraMessage of FreeFlyController.Message

    // tone-mapping
    | SetExposure of float
    | SetKey of float
    | SetExposureMode of ExposureMode

    | NOP
