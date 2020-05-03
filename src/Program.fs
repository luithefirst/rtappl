namespace Demo

open Aardium
open Aardvark.UI
open Suave
open Aardvark.Rendering.Vulkan
open Aardvark.Base
open Aardvark.Application.Slim

module Main =

    [<EntryPoint>]
    let main argv = 
        Aardvark.Init()
        Aardium.init()

        let app = new OpenGlApplication()

        WebPart.startServerLocalhost 4321 [
            MutableApp.toWebPart' app.Runtime false (App.start App.app)
        ] |> ignore
    
        Aardium.run {
            title "Aardvark rocks \\o/"
            width 1024
            height 768
            url "http://localhost:4321/"
        }

        0
