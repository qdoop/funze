module MyNamespace.zterm8evaluate
#nowarn "20"

open MyNamespace.Global
open MyNamespace.ErrorTexts
open MyNamespace.Helper
open MyNamespace.Common


open System
open System.Linq
open System.Reflection
open System.Diagnostics

// open MyNamespace.gstore0eval
// open MyNamespace.rdf0dataset
// open MyNamespace.walk7evalseq
open MyNamespace.ZInvoke
// open MyNamespace.zterm8handlers
open System.ComponentModel
open System.Runtime.CompilerServices

open MyNamespace.BuildIns
// open MyNamespace.BuildIns0Operators
// open MyNamespace.BuildIns1Operators


//THIS IS THE HEART OF EXECUTION
let dotTERM_Operators(op:string) (rs:Term list) (os:Term list) =
    try
        // dynamic resolve
        let inline resolve (op:string) (rs:Term list) (os:Term list) =
                let tp=os.Head.GetType()
                let ms=tp.GetMember(op)
                if   0<ms.Length && MemberTypes.Property=ms.[0].MemberType then
                    let res= tp.InvokeMember(op, BindingFlags.GetProperty,  null, os.Head, [||])
                    // let res= handleResult res
                    match res with
                        | :? (Term list*Term list) as res -> res
                        | :? (Term list)           as res -> (rs, res @os.Tail)
                        | _                               -> (rs, err0 (ERR_RESULT_FORMAT_NOTOK res) ::os)
                    
                elif 0<ms.Length && MemberTypes.Method=ms.[0].MemberType   then
                    let res= tp.InvokeMember(op, BindingFlags.InvokeMethod, null, os.Head, [|rs; os|]) //;;; separates arrays
                    match res with
                        | :? (Term list*Term list) as res -> res
                        // | :? (Term list)           as res -> (rs, res @os.Tail)
                        | _                                -> (rs, err0 (ERR_RESULT_FORMAT_NOTOK res) ::os)
                else
                    (rs, err0 (ERR_TERM_OPERATOR_NOTFOUND op) ::os)

        match op with
            | "+"       -> BOps.op_add  rs os
            | "-"       -> BOps.op_sub  rs os
            | "*"       -> BOps.op_mlt  rs os
            | "/"       -> BOps.op_div  rs os

            | "add"     -> BOps.op_add  rs os
            | "sub"     -> BOps.op_sub  rs os
            | "mlt"     -> BOps.op_mlt  rs os
            | "div"     -> BOps.op_div  rs os
            | "mod"     -> BOps.op_mod  rs os

            // | "op_gt"   -> BOps.op_gt   rs os
            // | "op_ge"   -> BOps.op_ge   rs os
            // | "op_lt"   -> BOps.op_lt   rs os
            // | "op_le"   -> BOps.op_le   rs os

            ////////////////////////////////////////PROPERTIES
            | "len"     -> (rs, os.Head.len @os.Tail)            
            // |   "mod"   -> os.Head.mod  rs os
            |  _        -> 
                           resolve op rs os //try dynamic resolve
    with
        | ex -> (rs, err0 (ERR_TERM_OPERATOR_EXCEPTION ex) ::os)




let rec dotMemberAccess (name:string) (rs:Term list) (target:obj) (ps:Term list) =
    let tp=target.GetType()
    let ms=tp.GetMember(name)
    if 0<ms.Length then 
        zlog ms.[0]
    try
        if     0<ms.Length&& MemberTypes.Property=ms.[0].MemberType then
            // printfn "readind property %s on type %A" name TypeDescriptor.GetProperties
            let res= tp.InvokeMember(name, BindingFlags.GetProperty, null, target, [||])
            let res= handleResult res
            (rs, res@ps)
        else if 0<ms.Length&& MemberTypes.Method=ms.[0].MemberType then
            // printfn "method lamda %s on type %A" name tp
            let f= fun pa -> 
                            // printfn "Invoke Method lamda %s on type %A" name tp
                            tp.InvokeMember(name, BindingFlags.InvokeMethod, null, target, pa)

            (rs,  F f ::ps)
        else
            match target with
                | :? Term as tt       ->   
                                dotMemberAccess name rs (tt.asObj) ps
                | _         ->   
                                (rs, err0 (ERR_OBJ_MEMBER_NOTFOUND name) ::ps) 
    with
        | ex -> (rs, err0 (ERR_OBJ_ACCESS_EXCEPTION ex) ::ps)



///////MAIN ENGINE ENRY POINT/////////////////////////////////////////////////
let rec term8eval (envs:Zenv list) (ts:Term list) (os:Term list) :Zenv list*Term list*Term list =


    envs.Head.log1 lvl "\n\n======\nengine start...\n"
    let mutable last_term=LT(0,V "nop")
    let rec next (envs:Zenv list) (ts:Term list) (os:Term list) :Zenv list*Term list*Term list=
        let sw=Stopwatch.StartNew()
        //UNGLY WAY TO BREAK EXECUTION
        // let ts  = if ""=( env.HasDbgInfo() )then ts else [err0 (env.HasDbgInfo())] 

        //TEST ERROR
        let printStack()=
            match os with
                | E e ::ps  ->  envs.Head.log1 lvl "!!!OOPS HAS ERRORS!!! Exiting Engine...\n"
                                envs.Head.log2 lvl "last_term: %A\n" last_term
                                match last_term with
                                    | CPTF ((p, t), f) ->
                                                    let s0,s1=linepos envs.Head.State.src p
                                                    let s2,s3=linepos envs.Head.State.src (p+4)
                                                    envs.Head.log6 lvl "#error=%A,%A,%A,%A|%A \n" s0 s1 s2 s3 last_term
                                    | _  ->         ()
                                ()
                | _         ->
                        let trim=3
                        let trim_os = List.truncate trim os        
                        match ts with
                            | W x :: _ ->   
                                            envs.Head.log1 2 "Whitespase ignored.\n"
                                            ()
                            | x   :: _ -> 
                                            last_term <- ts.Head
                                            envs.Head.log3 lvl   "%A___tos_____  << %O\n" envs.Length x //os.Length
                                            if 0=os.Length               then      envs.Head.log2 lvl ".  %O\n" "( *empty* )"
                                            else for i in [0..trim_os.Length-1] do envs.Head.log2 lvl ".  %O\n" os.[i]
                                            if trim_os.Length<os.Length  then      envs.Head.log2 lvl ".  %O\n" "..."

                                            ()
                            | _        -> 
                                            envs.Head.log3 lvl   "%A___tos__________%O__\n" envs.Length "!!!END!!!" //os.Length
                                            if 0=os.Length               then      envs.Head.log2 lvl ".  %O\n" "( *empty* )"
                                            else for i in [0..trim_os.Length-1] do envs.Head.log2 lvl ".  %O\n" os.[i]
                                            if trim_os.Length<os.Length  then      envs.Head.log2 lvl ".  %O\n" "..."

                                            //envs.Head.log2 lvl "<<<<EXEC t= [%s]  \n\n"   "!!!END!!!"   //trim_os
                                            ()

        printStack()

        let stepwait () =
            envs.Head.log2 lvl "(ms): %A\n\n" sw.Elapsed.TotalMilliseconds
            if 0<STEPWAIT then
                zlog "Sleeping..." 
                System.Threading.Thread.Sleep STEPWAIT
            ()

        // let stepwait x =
        //     env.log2 lvl "(ms): %A\n\n" sw.Elapsed.TotalMilliseconds
        //     if 0<STEPWAIT then System.Threading.Thread.Sleep STEPWAIT
        //     ()

        match  ts , os  with
        // |  W _ ::rs , _     ->
        //                         // stepwait()    
        //                         next env rs os

        | W _ ::V "clearerr" ::rs , E e ::ps ->   
                                    envs.Head.log1 lvl "REPL Dropped error\n" 
                                    stepwait()
                                    next envs rs ps
        | V "clearerr" ::rs , _ ->   
                                    stepwait()
                                    next envs rs os
        | V "exit" ::rs , _ ->   
                                    envs.Head.log1 lvl "EXIT!!!\n" 
                                    stepwait()
                                    (envs, rs, os)

        |  E e ::rs , _     ->        
                                stepwait()
                                (envs, ts, os)
        |  _   , E e ::ps   ->      
                                if "ok"=e then
                                    //ignore it
                                    envs.Head.log1 lvl ">>>>>>>>>>>>>>>>>>>>>>E \"ok\" ignored.\n" 
                                    stepwait()
                                    next envs ts ps
                                else      
                                    // stepwait()
                                    (envs, ts, os)

        |  W _ ::rs , _     ->  //////for proper error display Normaly should be very first
                                // stepwait()    
                                next envs rs os

        | CPTF ( _, f) ::rs, _   -> 
                                let zs, rs, os=f envs rs os
                                stepwait()    
                                next envs rs os

        | ( N _ |I _ |D _|L _|Y _ |X _|M "_" ) ::rs , _     ->        
                                let os= ts.Head ::os
                                stepwait()    
                                next envs rs os

        |  S s ::rs , _     ->      //fix strings depending on prefix   
                                let os= S s ::os
                                stepwait()    
                                next envs rs os

        // | Y y ::rs , _       ->      //see above
        //                             let os= Y y ::os
        //                             sleep()    
        //                             next env rs os
        // | M "."::rs,   Y z::Y y::Y x::ps ->
        //                         ge00.AddTriple x y z
        //                         stepwait()    
        //                         next env rs ps


        | V v ::rs, p ::ps  when 0=v.IndexOf "."     ->
                                dbgme v
                                let v=v.Substring 1
                                dbgme v
                                let rs,os= dotMemberAccess v rs p ps
                                stepwait()    
                                next envs rs os


        | G (".(",gs) ::rs, F f ::ps    ->
                                // dbgme gs
                                let _,rs0,os0=term8eval envs gs []

                                let pa=List.rev os0
                                // dbgme pa
                                let pa=List.map (fun (x:Term)-> x.asObj) pa
                                // dbgme pa
                                let pa= List.toArray pa
                                // dbgme pa
                                let qs= f pa //invoke lamda containing reflected method
                                // dbgme qs //IMPORTANT MAY CONSUME COLLECTION RESULTS
                                let qs= handleResult qs
                                // dbgme qs 
                                
                                stepwait()    
                                next envs rs  (qs @ ps)

        | G ("[",xs) ::rs, _          ->
                                stepwait()    
                                next envs rs (L xs :: os)        

        | G (".[",gs) ::rs, L xs ::ps -> //index access
                                dbgme gs
                                let _,rs0,os0=term8eval envs gs []

                                let pa=List.rev os0
                                dbgme pa
                                let pa=List.map (fun (x:Term)-> x.asNum) pa
                                dbgme pa

                                let qs=match pa with
                                        |[I i]          -> [ xs.[i] ]
                                        |[I i0; I i1]   -> [ L xs.[i0 .. i1] ]
                                        |  _            -> [ L [] ]
                                dbgme qs

                                stepwait()    
                                next envs rs (qs @ ps)


        | B op ::rs , _        -> //POWERFULL OPERATION
                                let rs,os= dotTERM_Operators op rs os
                                stepwait()    
                                next envs rs os
        | K k ::rs , O o ::ps   ->     
                                let rs,os= dotMemberAccess k rs o ps
                                stepwait()    
                                next envs rs os
        | K k ::rs , xo ::ps    -> 
                                let rs,os= dotMemberAccess k rs xo ps
                                stepwait()    
                                next envs rs os
        | H k ::rs , x ::ps     ->
                                ignore<| envs.Head.kvset k os.Head
                                stepwait()                                 
                                next envs rs os
        | R k ::rs , _          ->
                                let tt = envs.Head.kvget k 
                                let os = tt ::os
                                stepwait()          
                                next envs rs os


        | V "repl"::rs, _       ->    // Simple REPL LOOP  
                                let mutable os1=os
                                let mutable ts1=ts
                                REPLLOOP <- true
                                envs.Head.log1 10 "==REPL==LOOP====\n"
                                envs.Head.log2 lvl "os= %A\n"  os1 
                                while REPLLOOP do
                                    if 0=replTerms.Length then
                                        System.Threading.Thread.Sleep(0)
                                    else
                                        ts1       <- replTerms
                                        replTerms <- []
                                        let env0, rs0, os0= term8eval envs ts1 os1
                                        os1 <- os0
                                        envs.Head.log2 lvl "os= %A\n"  os1   
                                        envs.Head.log1 10 "==REPL==WAIT=MORE====\n"
                                        
                                envs.Head.log1 10 "==REPL==EXIT====\n"     
                                let os=os1
                                stepwait()    
                                next envs rs os                                

        //////////////////////////////////// V cases////////////////////////////
        // | V v::rs , _           ->
        //                         let env, rs, os = handleVterm v envs.Head rs os
        //                         stepwait()    
        //                         next envs rs os

        //////////////////////////////////// G cases//////////////////////////
        | G ("()", fa ) ::rs , F f ::ps ->
                                let env, xs, ys= term8eval envs fa []                                        
                                let pa:obj[]=List.map (fun (x:Term)-> x.asObj) ys                                                 
                                                |>  List.rev 
                                                |>  List.toArray                                
                                envs.Head.log2 10 "----f invoke  (%A) \n" pa
                                for p in pa do env.Head.log2 10 "%A \n" (p.GetType())
                                envs.Head.log1 10 "----f (pa)\n"
                                let res = (f) pa
                                envs.Head.log2 10 "----f (pa)= %A \n" res
                                let res = handleResult res
                                let os= res @ps
                                next envs rs os
        // | G _ ::rs , _          ->
        //                         let env, rs, os = handleGterm  envs.Head ts os
        //                         stepwait()    
        //                         next envs rs os



        ////////////////////////////////////////////////// M cases
        | M m::rs , _    ->
            match m , os with
                | "=" , S nm::va ::ps      ->  // =ASSIGN   
                                    let os=envs.Head.assign nm va ::ps
                                    stepwait()    
                                    next envs rs os 
                | m , _   when List.contains m ["+";"-";"*";"/"]->   
                                    let rs,os= dotTERM_Operators m rs os
                                    stepwait()    
                                    next envs rs os
                | m , _   when List.contains m ["{";"]";"[";"]";"(";")"]  ->      
                                    let os= err0 (ERR_UNBALANCED_GROUP ts.Head ) ::os
                                    stepwait()    
                                    next envs rs os
                | "," , _   ->      //IGNORE COMMAS   possible commaOn commaOff case
                                    stepwait()    
                                    next envs rs os
                | _ , _         ->  //catch rest
                                    ztrc 2 __SOURCE_FILE__ __LINE__
                                    let os= err0 (ERR_UNHANDLED_CASE ts.Head ) ::os
                                    next envs rs os  

        ////////////////////////////rare cases
        // | A("#",qtxt)::rs, _                ->      //trim it
        //                             let qtxt=qtxt.[2..qtxt.Length-2]                                                           
        //                             // zlog qtxt
        //                             // match SparqlPrepare qtxt with
        //                             match TriplesParse qtxt with
        //                                 | ("", Some q)  ->
        //                                                 envs.Head.log2 10 "%A\n" (q.GetType())
        //                                                 for x in q.Triples do envs.Head.log2 10 "%A\n" (x.GetType())
        //                                                 let os= O q::os
        //                                                 stepwait()
        //                                                 next envs rs os
        //                                 | (err, _ )     ->
        //                                                 let os= err0 (ERR_GENERIC_TXT err)::os
        //                                                 stepwait()
        //                                                 next envs rs os

        | t::rs , _         ->  
                                ztrc 2 __SOURCE_FILE__ __LINE__
                                let os= err0 (ERR_UNHANDLED_CASE ts.Head ) ::os
                                next envs rs os                   
        | _ , _             ->  
                                //envs.Head.log1 lvl "ok!\n"
                                // env.log2 lvl "ts= %A\n"  ts
                                // env.log2 lvl "os= %A\n"  os
                                (envs, ts, os)


    let envs,ts,os=next envs ts os

    envs.Head.log1 lvl "====engine done!!!\n\n"

    envs,ts,os