namespace Demo

open System
open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI.Primitives
open Aardvark.UI
open Aardvark.Data.Photometry
open Adaptify

type ExposureMode = Manual=0 | MiddleGray=1 | Auto=2
    
type RenderMode =
    | GroundTruth = 0
    | Cubature = 1
    | Compare = 2

type GTSamplingMode =
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
        gtSamplingMode       : GTSamplingMode
        gtSamplesPerFrame    : int
        gtNoAccumulation     : bool
        
        // tonemapping
        exposureMode    : ExposureMode
        exposure        : float
        key             : float

        cameraState     : CameraControllerState
    }



type Message =
    //| CHANGE_RENDER_MODE of RenderMode
    //| TOGGLE_LTC_SPECULAR

    //| SET_GT_SAMPLINGMODE of GTSamplingMode
    //| SET_GT_SAMPLESPERFRAME of float
    //| TOGGLE_GT_NOACCUMULATION

    //| IMPORT_PHOTOMETRY of string
    //| TOGGLE_USE_PHOTOMETRY
    //| CHANGE_DIFFUSE_EXITANCE of float

    //| CHANGE_LIGHT_TRANSFORM_MODE of LightTransformMode
    //| TRANSLATE_LIGHT of int * V3d // lightID, direction
    //| ROTATE_LIGHT of int * V3d // lightID, euler angles

    //| SET_TM_EXPOSURE of float
    //| SET_TM_KEY of float
    //| SET_TM_EXPOSUREMODE of Option<ExposureMode>

    | SetRenderMode of RenderMode option
    | ToggleLTCSpecular

    | LoadPhotometry of list<string>
    | ResetPhotometry
    | ToggleUsePhotometry 
    | SetDiffuseExitance of float

    | ChangeLightTransformMode of LightTransformMode
    | ResetLightTransform
    | TranslateLight of V3d
    | RotateLight of V3d

    // tone-mapping
    | SetExposure of float
    | SetKey of float
    | SetExposureMode of Option<ExposureMode>

    | CameraMessage of FreeFlyController.Message

    | NOP

//type Message =
//    | CameraMessage of FreeFlyController.Message
//    | SetExposure of float
//    | SetKey of float
//    | SetExposureMode of Option<ExposureMode>
//    | Nop