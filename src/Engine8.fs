module MyNamespace.Engine8
open System

#nowarn "20"

open MyNamespace.Global
open MyNamespace.ErrorTexts
open MyNamespace.Helper
open MyNamespace.Common

// open Data0.JsonFlex

// open System
// open System.Linq
// open System.Reflection
open System.Diagnostics
// open System.Text


// open MyNamespace.lite7stor
// open MyNamespace.Vocabulary
// // open MyNamespace.gstore0eval
// open MyNamespace.rdf0dataset
// open MyNamespace.ZInvoke
// open MyNamespace.zterm7eval


// open MyNamespace.gnode7stor

// open MyNamespace.gstore8common
// open MyNamespace.gstore8edge
// open MyNamespace.gstore8pathpath

// open System.ComponentModel

// open MyNamespace.BuildIns
// open MyNamespace.BuildIns0Operators
// open MyNamespace.BuildIns1Operators
// open System.Threading
// open System.Threading.Tasks
// open MyNamespace.Control

(*
let envstats lvl =
    glog1 lvl "---------------------\n"
    glog2 lvl "WorkingSet  = %A     \n" (System.Environment.WorkingSet/1_000_000L)
    glog2 lvl "Triple Count= %A  (K)\n" ((float gTripleCount  ) /1000.0)
    // glog2 lvl "Cell   Count= %A  (K)\n" ((float stor.CellCount) /1000.0)
    glog1 lvl "\n"




// type ddd =
//     static member f (x, [<ParamArray>](x1:obj[]) ) =printfn "%A" x1

let tdb=TxtStore("test0.dbz")
let InitEngineEnviroment () = 
    glog1 lvl "engine8: InitEngineEnviroment()...\n"
    // envstats lvl
    // Vocabulary.LoadPredicates()

    gdb.DropCollection "nid2nod"
    PersistNodes()
    // listNodes()
    
    gid2uriMap.TryAdd( 300, "p:args" )
    uri2gidMap.TryAdd( "p:args", 300 )


    zlog agent //First Access to type from module runs initialization



    MyNamespace.gstore8edge.PrepareGraph8()
    
    // envstats lvl
    glog1 lvl "engine8: InitEngineEnviroment() ok!\n"
    ()


let PrepareNewEngine (src:string) logger :JsonFlex =
    
    let src=src+"\r\n"

    let name="ngx0_" + DateTime.UtcNow.ToString("yyMMdd_HHmmss_fff")
    let state=  {   newEnvState with
                        desc="";
                        src = src;
                }

    let env=new Zenv(name, zterm8evaluate.term8eval, state)
    env.SetLogger(logger)

    
    //Tokenize
    let xs=MyNamespace.zscript8tokenizer.token8script env src
    // if src = List.fold (fun s x-> s + (snd x)) "" xs then
    //     glog1 lvl "Tokenizer Validate ok!\n"
    // else
    //     glog1 lvl ">>>>>Tokenizer Validate Failed!\n" 
    //Parse
    let ts=MyNamespace.zscript8parser.parse8script xs

    StartNewEngine env ts []


*)



let demo8evalScript (script:string) logger =
    glog1 lvl "\ndemo8evalScript() ...\n"
    glog1 lvl "Engine      = term8eval engine.\n"
    glog2 lvl "WorkingSet  = %A    \n" (System.Environment.WorkingSet/1_000_000L)
    glog2 lvl "Triple Count= %A (K)\n" ((float gTripleCount  ) /1000.0)
    // glog2 lvl "Cell   Count= %A (K)\n" ((float stor.CellCount) /1000.0)
    glog1 lvl "\n"

    let sw=Stopwatch.StartNew()
    
    let state=  { newEnvState with  desc=""; src=script }
    let env=new Zenv("xxx", state, MyNamespace.zterm8evaluate.term8eval, logger )
    // env.SetLogger(logger)
    // env.Eval2 <- MyNamespace.zterm8evaluate.term8eval

    // let script=script + "\r\n"
    //Tokenize
    let xs=MyNamespace.zscript8tokenizer.token8script env script

    // if (script + "\r\n") = List.fold (fun s x-> s + (snd x)) "" xs then
    //     glog1 lvl "Tokenizer8 Validate ok!\n"
    // else
    //     glog1 lvl ">>>>>Tokenizer8 Validate Failed!\n" 

    //Parse
    let ts=MyNamespace.zscript8parser.parse8script xs

    glog2 lvl "lexer parse elapsed %A\n" sw.Elapsed.TotalMilliseconds


    let sw=Stopwatch.StartNew()

    try
        // Evaluate  
                      
        let envs, ts, os = env.Eval2 [env] ts []

        // glog2 5 "__ts__= %s"  (printRawStack 1 ts)
        // glog2 5 "__os__= %s"  (printRawStack 4 os)

        glog2 lvl "Total elapsed %A (ms)\n" sw.Elapsed.TotalMilliseconds
        // envstats lvl
       
        // zlog (( Seq.map Gid2Txt ( Path8.Sources().OutV([]).Iter ) ).ToArray())
        
        // zlog "results"
        // for z in Path8.Sources().OutV(["is"]).Iter do
        //      zlog z.txt        
        // printfn "__@__x%016X" ( MyNamespace.Helper.xx2cid 0xFFFF_FFFFu 2u )

        let json = match ts,os with
                    | _ , E e ::ps      ->  
                            let json="""{ "error":true; "result":[] }"""
                            json

                    // | [], Tz ds ::ps  ->
                    //         glog1 lvl "\n__json_return_______\n"
                    //         // envstats lvl
                    //         let sw=Stopwatch.StartNew()
                    //         glog1 lvl "Sequencing results...\n"                

                    //         let cnt, json=visualizeGraphSeq ds logger
                    //         let json =if ""<>env.HasDbgInfo() then env.HasDbgInfo() else json
                    //         glog2 lvl "Edge   Count= %d\n" cnt
                    //         // envstats lvl 
                    //         json


                    // | [], Tvvv t ::ps  ->
                    //         glog1 lvl "\n__json_return_______\n"
                    //         // envstats lvl
                    //         let sw=Stopwatch.StartNew()
                    //         glog1 lvl "Forcing results...\n"                
                    //         ignore<| t.ws.Force()
                    //         let cnt, json=visualizeGraph t.ws.Value logger
                    //         let json =if ""<>env.HasDbgInfo() then env.HasDbgInfo() else json
                    //         glog2 lvl "Edge   Count= %d\n" cnt
                    //         // envstats lvl 
                    //         json
                    |  _,_        ->
                            glog1 lvl "\n__json_return_______\n"               
                            let json="""
                            {"result":[]}
                            """
                            let json =if ""<>envs.Head.HasDbgInfo() then envs.Head.HasDbgInfo() else json
                            // glog1 lvl json
                            json    

        glog1 lvl "\ndemo8evalScript() ok!\n"
        glog2 lvl "Total elapsed %A (ms)\n" sw.Elapsed.TotalMilliseconds
        //envstats lvl
        json

    with
        | ex   ->  
                glog2 10 "EVALUATION EXCEPTION:\n%A \n" ex        
                """ { "error":"ENGINE CRASH", "result":[] } """







                