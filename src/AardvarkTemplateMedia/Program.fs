open AardvarkTemplateMedia

open Aardium
open Aardvark.UI
open Suave
open Aardvark.Rendering.Vulkan
open Aardvark.Base

[<EntryPoint>]
let main args =
    Aardvark.Init()
    Aardium.init()

    let app = new HeadlessVulkanApplication()

    WebPart.startServer 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start App.app)
    ] |> ignore
    
    Aardium.run {
        title "Aardvark rocks \\o/"
        width 1024
        height 768
        url "http://localhost:4321/"
    }

    0
