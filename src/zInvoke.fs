module MyNamespace.ZInvoke

#nowarn "20"

open MyNamespace.Global
open MyNamespace.Helper
open MyNamespace.Common

open System
open System.IO
open System.Reflection

// zlog "==TEST COMPILE SERVICES============"
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Interactive.Shell


let CompileFsharpFragment (code:string) :Term list=
    let checker = FSharpChecker.Create()
    let fn      = Path.GetTempFileName()
    let fn2     = Path.ChangeExtension(fn, ".fsx")
    let fn3     = Path.ChangeExtension(fn, ".dll")
    let tmpl="""
// # r "C:\Program Files\dotnet\shared\Microsoft.NETCore.App\2.2.7\System.dll"
// # r "C:\Program Files\dotnet\shared\Microsoft.NETCore.App\2.2.7\mscorlib.dll"


open System
open FSharp.Core
open MyNamespace.Global
open MyNamespace.Common

#line 9998 
let InvokeWrap_0000 () =   
    // begin //// embed code  
    __CODE__0000    
    // end   //// embed code
let InvokeWrap_0000_CastToObj () : System.Object =  box( InvokeWrap_0000 () )  
"""
    let usercode=tmpl.Replace("__CODE__0000",code.Replace("\n","\n    ") )
    glog2 lvl "%s" usercode
    File.WriteAllText(fn2, usercode)
    try
        let s1 = checker.CompileToDynamicAssembly([| "-o"; fn3; "-a"; "-r"; Assembly.GetExecutingAssembly().Location; fn2 |], execute=None)
        let info, exitcode, assy=  Async.RunSynchronously s1

        if assy.IsNone then
            glog2 10 "\n==COMPILE ERROR:====\n%A \n" info 
            [ E (sprintf " COMPILE ERROR\n: %A" info) ]
        else
            let m0=assy.Value.GetLoadedModules().[0]
            let t0=m0.GetTypes().[0]
            // zlog "compile OK"
            // let res=t0.InvokeMember("InvokeWrap_0000", BindingFlags.InvokeMethod, null, null, [||])
            // let res=box res
            let res=t0.InvokeMember("InvokeWrap_0000_CastToObj", BindingFlags.InvokeMethod, null, null, [||])
            // zlog res
            // zlog <| (res:?>unit->unit)()
            // glog2 10 "~~~~~~~~~~~~%A" (res.GetType())

            handleResult res
    with 
        | ex -> [ E (ex.ToString()) ]




// PLEASE CHECK Compiler Services: Virtualized File System //for an in memory FileSystem
let testIntreactive()=

    let collectionTest() = 

        for i in 1 .. 1 do
            let defaultArgs = [|"fsi.exe";"--noninteractive";"--nologo";"--gui-"|]
            use inStream = new StringReader("")
            use outStream = new StringWriter()
            use errStream = new StringWriter()

            let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
            use session = FsiEvaluationSession.Create(fsiConfig, defaultArgs, inStream, outStream, errStream, collectible=true)
            
            session.EvalInteraction (sprintf "type D = { v : int }")
            let v = session.EvalExpression (sprintf "{ v = 42 * %d }" i)
            // let v = session.EvalExpression ("zlog")
            printfn "iteration %d, result = %A" i v.Value.ReflectionValue

    collectionTest()


let testInvoke()=
    // // Create an interactive checker instance 
    let checker = FSharpChecker.Create()
    let fn      = Path.GetTempFileName()
    let fn2     = Path.ChangeExtension(fn, ".fsx")
    let fn3     = Path.ChangeExtension(fn, ".dll")

    File.WriteAllText(fn2, """
    // module M
    // type C() = 
    //     static member P() = 
    //         printfn "%A" "qqqqqqqqqqqqqqqqqqqqqqqq"
    //         55555
    // let x = 3 + 4
    // let dddd=fun (x:string) ->  x + "ok!" 
    open MyNamespace.Global
    let test1 = 
    begin
    fun () -> 
        MyNamespace.Zengine.checkGEAvail()
        zlog "dwww//open MyNamespace.Global to work!"  //open MyNamespace.Global
        "sswwwwwwwwwwgggggggggggggwwwwwwsss"
    end


    """)
    // zlog fn2
    // // let s0= checker.Compile([| "fsc.exe"; "-o"; fn3; "-a"; fn2 |])
    // // zlog <| Async.RunSynchronously s0

    let s1 = checker.CompileToDynamicAssembly([| "-o"; fn3; "-a"; "-r"; @"C:\zzWORK_ESP32\FSHARP\fz0demo\bin\Debug\netcoreapp2.0\fz0demo.dll"; fn2 |], execute=None)
    let info, exitcode, assy=  Async.RunSynchronously s1
    zlog "======================="
    printfn "%A" assy

    zlog "------xxx-----"
    assy.Value.GetLoadedModules()
    printfn "%A" (assy.Value.GetLoadedModules())

    let mo=assy.Value.GetLoadedModules().[0]
    printfn "mo=%A" mo

    printfn "moduleTypes=%A" (mo.GetTypes())
    let t0=mo.GetTypes().[0]
    printfn "tt=%A" t0
    let t1=mo.GetTypes().[1]
    printfn "tt=%A" t1


    printfn "fx=%A" ( t0.GetMembers())


    // let fot=typeof<fo>
    // printfn "%A" (fo.GetTypes())

    // let res=t0.InvokeMember("P", BindingFlags.InvokeMethod, null, null, [||])
    let res=t0.InvokeMember("test1", BindingFlags.InvokeMethod, null, null, [||])
    zlog res

    // printfn "%A" (AppDomain.CurrentDomain.GetAssemblies())
    // let x:obj = Json.fromJson( Encoding.UTF8.GetBytes("{\"a\":1,\"b\":[0,1]}")    )
    // zlog <| x.GetType().GetProperties()

    // let txt="{\"r\":{\"a\":1,\"b\":[0,1]}}"
    // // let mutable trg=new Object()
    // // let trg=Newtonsoft.Json.JsonConvert.DeserializeXNode(txt)
    // // zlog trg
    // // for x in trg.Nodes() do
    // //     zlog x
    // type TT = {x:int;y:int;z:int list;}

    // let trg=Newtonsoft.Json.JsonConvert.SerializeObject({x=1;y=2;z=[];})
    // zlog trg
    // // zlog <| trg.GetType().GetMembers()

    // let xx=Newtonsoft.Json.JsonConvert.DeserializeObject<TT> """{"x":1,"y1":12, "z":[1,2,3]}"""
    // zlog xx


    zlog "DONE"