module Webbox

#nowarn "20"

// dotnet restore -s c:\Users\rain2\.nuget\packages
//  nodemon -e fs,html -w standalone -w . -w Program.fs --exec "dotnet run --no-restore"

open System
open System.Text
open System.Net
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Collections.Concurrent
open System.Threading
open Suave
open Suave.CORS
open Suave.Filters
open Suave.Operators
open Suave.Logging
open Suave.Json
open System.Runtime.Serialization
open FSharp.Data
open System.Collections.Concurrent

open Newtonsoft.Json

open MyNamespace.Global
open Scriban
open MyNamespace.Common


open FSharp.Data

let infoJ =
  JsonValue.Parse(""" 
    { "name": "Tomas", "born": 11111933333555599777972,
      "siblings": [ "Jan", "Alexander" ] } """)
//printfn "json sample %A" infoJ

type Hcxt = {
                baseurl:string

                mutable xs:obj
            }
let hcxt = { 
                baseurl = "http://localhost:7071/api/"
                // member x.postfix = "?vargs=status"
                // // member x.env = envvars.ToArray()
                // member x.cwd = Environment.CurrentDirectory
                // member x.assy = Assembly.GetExecutingAssembly()
                // member x.fver = Assembly.GetExecutingAssembly().FullName
                // member x.lver = Assembly.GetAssembly(typeof(LibraryA.Say)).FullName
                // member x.name = new { x = "x1", y = "y2" }
                // member x.z = new[] { 1, 3 }
                // member x.filepaths = fundirs.ToArray()
                //member x.dummy = "dummy"
                xs=[||]
            }

 

let stream00=new ConcurrentQueue<ZStream>()


let zlog z=
    printfn "server: %A" z
    ()


let queryWebHandler0 (x:HttpContext) :WebPart =
    printfn "xxxxxx%A" x
    Successful.OK @"{""r"":""ok""}"

let mutable log0:string list=[]
let fzLogger=
    fun z k v->
        log0 <- log0 @ [v]
        stream00.Enqueue {t=DateTime.UtcNow; z=z; k=k; v=v;}
        ()
let query0WebHandler1 milliseconds message: WebPart =
    fun (cxt : HttpContext) ->  async {
        
        // do! Async.Sleep milliseconds
        let txt0= message + Text.Encoding.UTF8.GetString (cxt.request.rawForm)

        // do! Async.Sleep milliseconds
        fzLogger "query0WebHandler1" "_" "\n__QUERY_____________\n"
        let txt1= (MyNamespace.Engine8.demo8evalScript txt0  fzLogger)
        fzLogger "query0WebHandler1" "_" "\n__READY_____________\n"

        return! Successful.OK txt1 cxt
    }

let replWebHandler1 milliseconds message: WebPart =
    fun (cxt : HttpContext) ->  async {
        
        do! Async.Sleep 500 //milliseconds
        let txt0= message + Text.Encoding.UTF8.GetString (cxt.request.rawForm)

        // do! Async.Sleep milliseconds
        // fzLogger "\n__QUERY_____________\n"
        // let txt1= (MyNamespace.zdemo7main.demo7evalScript txt0  fzLogger)
        // fzLogger "\n__READY_____________\n"
        // if MyNamespace.Global.REPLLOOP then
        //     let xs=MyNamespace.zterm7read.token7script txt0
        //     let ts=MyNamespace.zterm7read.parse7script xs
        //     while 0<>MyNamespace.Common.replTerms.Length do
        //         System.Threading.Thread.Sleep(0)
        //     MyNamespace.Common.replTerms <- ts

        return! Successful.OK "ok1" cxt
    }

let runsWebHandler1 milliseconds message: WebPart =
    fun (cxt : HttpContext) ->  async {

        // do! Async.Sleep milliseconds
        let txt0= message + Text.Encoding.UTF8.GetString (cxt.request.rawForm)

        glog2 5 "runsWebHandler1: %A\n" DateTime.UtcNow
        // do! Async.Sleep milliseconds
        // let txt1= (MyNamespace.zdemo7main.demo7evalScript txt0  fzLogger)
        let txt1= (MyNamespace.Engine8.demo8evalScript txt0  fzLogger).ToString()

        
        fzLogger "runsWebHandler1" "_" "\n__READY_____________\n"

        return! Successful.OK txt1 cxt
    }

let logsWebHandler1 milliseconds message: WebPart =
    fun (cxt : HttpContext) ->  async {
        // do! Async.Sleep milliseconds        
        // zlog "hit"
        let txt0= message + Text.Encoding.UTF8.GetString (cxt.request.rawForm)

        let chunk=log0
        log0<-[]

        // let sb=new StringBuilder()
        // List.map (fun (s:string)-> sb.Append(s)) chunk
        let sb=new StringBuilder()
        sb.AppendLine "["
        for i in [0..1000] do
            let ok, r= stream00.TryDequeue()
            if ok then
                let s0=r.v.Replace("\\","\\\\").Replace("\"", "\\\"").Replace("\n","\\n").Replace("\r","\\r").Replace("\t","\\t")
                sb.AppendLine (sprintf """{"t":"%A", "z":"%s", "k":"%s", "v":"%s"},""" r.t r.z r.k s0 ) 
            ()
        sb.AppendLine """{"t":"", "z":"", "k":"", "v":""}]"""
        // let txt1= txt + ( sprintf "%A" DateTime.UtcNow ) + "\n----\n"
        // let txt1= Rdfbox.queryRDF txt
        let txt1=sb.ToString()

        return! Successful.OK txt1 cxt
    }

let JsonLWebHandler1 milliseconds message: WebPart =
    fun (cxt : HttpContext) ->  async {
        // do! Async.Sleep milliseconds        
        // zlog "hit"
        let txt0= message + Text.Encoding.UTF8.GetString (cxt.request.rawForm)

        let sb=new StringBuilder()
        for i in [0..1000] do
            let ok, r= streamJSONL.TryDequeue()
            if ok then
                let s0=r //.Replace("\\","\\\\").Replace("\"", "\\\"").Replace("\n","\\n").Replace("\r","\\r").Replace("\t","\\t")
                sb.AppendLine (s0 + ",")
            ()

        let txt1=sb.ToString()

        return! Successful.OK txt1 cxt
    }

(*
let enginesHandler1 (tag:string): WebPart =
    fun (cxt : HttpContext) ->  async {

        let names=[|
                    for m in MyNamespace.Control.engineInstances do  
                        let json=JsonFlex.Empty
                        let json=json?name<-m.Value.Name
                        let json=json?tstatus <- (sprintf "%A" m.Value.State.task.Status)
                    
                        yield json
                   |]
        let json=JsonFlex.Cast names
        let json=json.AsVal.ToString()

        hcxt.xs <- names

        let tmpl = @"
<!DOCTYPE html>            
<html>
<head>
    <style>
        body {
            font-family: monospace;
        }
        h1 {
            color: maroon;
            margin-left: 40px;
        }
    </style>
</head>
<body> 

<h1>Instances</h1>
<ul>
{{ for x in xs }}
    <li> {{ """" + x }} </li>
{{ end }}
</ul>

</body>
</html>
        "

        let html=Template.Parse(tmpl).Render(hcxt)

        // return! Successful.OK html cxt
        return! Successful.OK json cxt
    }



let engnHandler1 (tag:string): WebPart =
    fun (cxt : HttpContext) ->  async {

        let name=cxt.request.path.Replace("/api/v1/engine/","")
        let engn=MyNamespace.Control.engineInstances.TryFind name

        if engn.IsNone then
            return! Successful.OK "{}" cxt        
        else
            // let names=[|for m in MyNamespace.Control.engineInstances do  yield m.Key|]
            // let json=JsonFlex.Cast names
            // let json=json.AsVal.ToString()
            let json=JsonFlex.Empty
            let json=json?name<-name
            let json=json?complete<-engn.Value.State.task.IsCompleted
            let json=json?tstatus<-(sprintf "%A" engn.Value.State.task.Status)

            let json=json.AsVal.ToString()
            // return! Successful.OK html cxt
            return! Successful.OK json cxt
    }

*)
let uploadsHandler (tag:string): WebPart =
    fun (cxt : HttpContext) ->  async {

        zlog Environment.CurrentDirectory
        zlog "========================"
        zlog cxt //.request.url

        for fe in cxt.request.files do
            if "application/x-zip-compressed"=fe.mimeType then
                System.IO.Compression.ZipFile.ExtractToDirectory(fe.tempFilePath, "_DATAPOOL",false) 
            ()

        return! Successful.OK "{}" cxt
    }











let wlog (tag:string): WebPart =
    fun (cxt : HttpContext) ->  async {

        zlog "=0======================="//////////////
        zlog cxt.request.url.LocalPath

        return Some cxt
    }


let testscan (t) : WebPart =
    fun (cxt : HttpContext) ->  async {
        zlog t
        return Some cxt
    }


let opts=InclusiveOption<string list>.All
let allcors=CORS.cors {defaultCORSConfig with allowedUris=opts; } 










let app : WebPart =     
    let botprefix="/v3/botstate/emulator/"
    choose [
        OPTIONS >=> pathStarts "/" >=>  allcors       
        


      ///  GET  >=> path "/standalone/third-party/monaco-editor/min/vs/loader.js"       >=> Files.file "./standalone/third-party/monaco-editor/min/vs/loader.js"


        GET  >=> path "/"        >=> Files.file "./standalone/query_sparql.html"


        GET  >=> pathStarts "/sss"             >=>  wlog "zzz"     >=> (Successful.OK ("xxxxx") )
        GET  >=> pathScan "/zzz%s"  testscan   >=>  wlog "zzz"     >=> (Successful.OK ("yyyyy") )

        POST >=> path "/api/v1/query0"     >=> allcors  >=> query0WebHandler1   1 ""
        POST >=> path "/api/v1/repl/0"     >=> allcors  >=> replWebHandler1     1 ""
        POST >=> path "/api/v1/runs/0"     >=> allcors  >=> runsWebHandler1     1 ""
        GET  >=> path "/api/v1/logs/0"     >=> allcors  >=> logsWebHandler1     1 ""
        GET  >=> path "/api/v1/jsonL/0"    >=> allcors  >=> JsonLWebHandler1    1 ""


        // GET  >=> path "/api/v1/engines"          >=> allcors  >=> enginesHandler1 ""
        // GET  >=> pathStarts "/api/v1/engine/"    >=> allcors  >=> engnHandler1 ""

        POST >=> path "/api/v1/write/file/nquad" >=> allcors >=>  uploadsHandler "zzz"  


        GET  >=>   Files.browseHome
        
        wlog "zzzz" >=> Suave.RequestErrors.BAD_REQUEST "SERVER ERROR"

        RequestErrors.NOT_FOUND "Page not found." 
    ]
    

[<EntryPoint>]
let main argv = 

    GLogger <- fzLogger
    // MyNamespace.zdemo8main.InitEngineEnviroment()

    let cts = new CancellationTokenSource()
    let conf = { 
        defaultConfig with 
            cancellationToken = cts.Token
            // homeFolder = Some (Path.GetFullPath "./public")
            homeFolder = Some  __SOURCE_DIRECTORY__ //(Path.GetFullPath "./standalone")


            
    }

    let listening, server = startWebServerAsync conf app //(Successful.OK "Hello World")

    Async.Start(server, cts.Token)

    while true do
        Thread.Sleep(10000)
    cts.Cancel()
    0 // return an integer exit code


// [<EntryPoint>]
// let main1 argv =

//     test8script ()
    
//     0
