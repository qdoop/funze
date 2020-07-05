module MyNamespace.zscript8cptf

open System.Collections
open System.Reflection
open System.Runtime.InteropServices

open MyNamespace.Global
open MyNamespace.Helper
open MyNamespace.Common
open MyNamespace.ErrorTexts

open MyNamespace.BuildIns
// open MyNamespace.gstore8edge
open MyNamespace.ZInvoke



let rec dotMemberAccess2 (target:obj) (name:string)=
    let tp=target.GetType()
    let ms=tp.GetMember(name)
    if 0<ms.Length then 
        zlog ms.[0]
    try
        if 0<ms.Length&& MemberTypes.Property=ms.[0].MemberType then
            // printfn "readind property %s on type %A" name TypeDescriptor.GetProperties
            let res= tp.InvokeMember(name, BindingFlags.GetProperty, null, target, [||])
            let res= handleResult res
            res.Head
        else if 0<ms.Length&& MemberTypes.Method=ms.[0].MemberType then
            // printfn "method lamda %s on type %A" name tp
            let f= fun pa -> 
                            // printfn "Invoke Method lamda %s on type %A" name tp
                            tp.InvokeMember(name, BindingFlags.InvokeMethod, null, target, pa)

            F f
        else
            match target with
                | :? Term as tt ->   
                                dotMemberAccess2 (tt.asObj) name
                | _             ->   
                                err0 (ERR_OBJ_MEMBER_NOTFOUND name) 
    with
        | ex ->  
                err0 (ERR_OBJ_ACCESS_EXCEPTION ex)


let mutable knownWords:Map<string, Zenv list->Term list->Term list -> Zenv list*Term list*Term list>=
    new Map<string, Zenv list->Term list->Term list -> Zenv list*Term list*Term list>(Seq.empty)


let initKnownWords()=
    // testme()

    knownWords  <- knownWords.Add("eval",fun (envs:Zenv list) (ts:Term list) (os:Term list) -> 
                                            let (G(m,gs)) =os.Head
                                            assert("{"=m)
                                            let newenvs=envs.Head.Clone ::envs
                                            let newenvs, xs, os= newenvs.Head.Eval2 newenvs gs os.Tail                                            
                                            envs, ts, os  
                                            )
    knownWords  <- knownWords.Add("nop",fun (envs:Zenv list) (ts:Term list) (os:Term list) -> 
                                            envs, ts, os   
                                            )

    // knownWords  <- knownWords.Add("ge",fun (envs:Zenv list) (ts:Term list) (os:Term list) -> 
    //                                         envs, ts, O (ge())::os   )

    // knownWords  <- knownWords.Add("g",fun (envs:Zenv list) (ts:Term list) (os:Term list) -> 
    //                                         envs, ts, O (new Tinker.BasicTraversalSource())::os  )

    // knownWords  <- knownWords.Add("gg",fun (envs:Zenv list) (ts:Term list) (os:Term list) -> 
    //                                         envs, ts, O (new Tinker.BasicTraversal())::os )
    
    knownWords  <- knownWords.Add("pathlist",fun (envs:Zenv list) (ts:Term list) (os:Term list) -> 
                                                let pp=(os.Head.asObj :?> IEnumerable)
                                                for p in pp do
                                                    // zlog p.txt
                                                    dbgme p
                                                    envs.Head.log2 10 "pathlist0: %A\n" p
                                                envs, ts, os 
                                                )
     
  
    
    
    ()
initKnownWords()




let CPTF_copy_to_os (t:Term) =
    
    fun (envs:Zenv list) (ts:Term list) (os:Term list) ->
        envs, ts, t::os

let CPTF_simplemath (op:string)=
    fun (envs:Zenv list) (ts:Term list) (os:Term list) ->
        if "+"=op then
            envs,ts, BOps.op_add2 os
        elif "-"=op then
            envs,ts, BOps.op_sub2 os
        elif "*"=op then
            envs,ts, BOps.op_mlt2 os
        elif "/"=op then
            envs,ts, BOps.op_div2 os
        else
            envs,ts, E "UNHANDLED CASE" ::os

let CPTF_dict_lookup (k:string) =
    let v=knownWords.TryFind k
    if v.IsSome then
        v.Value
    else 
        fun (envs:Zenv list) (ts:Term list) (os:Term list) ->
            envs, ts, E "MISSING WORD" ::os

let CPTF_member_access (k:string) =
    fun (envs:Zenv list) (ts:Term list) (os:Term list) ->
        envs, ts, dotMemberAccess2 os.Head k ::os.Tail
let CPTF_member_Invoke (gs:Term list) =
    fun (envs:Zenv list) (ts:Term list) (os:Term list) ->
        dbgme "xxxxxxx"
        let (F f) =os.Head
        let newenvs=envs.Head.Clone ::envs
        let newenvs, xs, ys= newenvs.Head.Eval2 newenvs gs []
        dbgme ys                                        
        let pa:obj[]=List.map (fun (x:Term)-> x.asObj) ys                                                 
                        |>  List.rev 
                        |>  List.toArray                                
        envs.Head.log2 10 "----f try invoke with args (%A) \n" pa
        for p in pa do envs.Head.log2 10 "%A \n" (p.GetType())
        // envs.Head.log1 10 "----f (pa)\n"
        // let res = (os.Head.asObj:?>(obj[]->obj)) pa
        let res= f pa
        // envs.Head.log2 10 "----f (pa)= %A \n" res //NOTE!!! THIS CONSUMES GRAPH QUERY NODES
        let res = handleResult res
        let os= res @os.Tail
        envs, ts, os

let CPTF_compile_fsharp (fs:string) =
    fun (envs:Zenv list) (ts:Term list) (os:Term list) ->
        let os= CompileFsharpFragment( fs ) @os
        envs, ts, os 



 

 
///////////////////////////////////////////////////////////////////////////////
let pcollect (mode:string) (arg:int*Term) : Term =    
    if ""=mode || "{"=mode || ".("=mode then
        match arg with
            | p, M m when -1<"+-*/".IndexOf(m)->  
                                        CPTF (  arg, CPTF_simplemath m      )
            | p, V v    ->              CPTF (  arg, CPTF_dict_lookup v     )
            | p, K k    ->              CPTF (  arg, CPTF_member_access k   )
            | p, G (".(", gs)->         CPTF (  arg, CPTF_member_Invoke gs  )
            | p, G ( "[", gs)->         CPTF (  arg, CPTF_copy_to_os (L gs) )
            | p, A ( "#", fs)->         CPTF (  arg, CPTF_compile_fsharp fs )
            | p, x      ->              CPTF (  arg, CPTF_copy_to_os x      )
            | _         ->
                            E "UNHANDLED CASE"
    else
        match arg with
            // | p, G ("[", gs)->          CPTF (  (p,snd arg), CPTF_copy_to_os (L gs) )
            | p, x      ->              snd arg  
            // | _         ->
            //                 E "UNHANDLED CASE"

            