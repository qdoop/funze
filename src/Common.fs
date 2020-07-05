module MyNamespace.Common
open System.Collections
//open gstore0lib
#nowarn "20" "67"

open MyNamespace.Global
open MyNamespace.ErrorTexts
open MyNamespace.Helper

open System
open System.Runtime
open System.Reflection
open System.Text
open System.Collections
open System.Collections.Generic

open System.Threading
open System.Threading.Tasks

type Pair=string*string
type Token=string*string

// let err0 (errlvl, errcode, errtxt) =
//     glog2 10 "\n\nE: %s\n\n" errtxt
//     E errtxt
let err1 (errlvl, errcode, errtxt) =
    glog2 10 "\n\nE: %s\n\n" errtxt
    errtxt

type ZEnvState ={ 
        level:int
        penvs:Zenv list
        cenvs:Zenv list
        src:string
        mutable lgr:(string->string->string->unit)
        mutable desc:string
        mutable LOGLEVEL:int
        mutable ERRLEVEL:int
        mutable STEPWAIT:int
        mutable REPLLOOP:bool
        mutable nsStack:Map<string,Term> list
        mutable kvStack:Map<string,Term> list
        mutable cts :CancellationTokenSource
        mutable task:Task
    }
and Zenv(name:string, state0:ZEnvState, evalfun, logger) =
    // let mutable exec=null
    // let envname="name0000"
    let mutable state={state0 with level=state0.level + 1; lgr=logger}
    let mutable dbginfo="" 
    let mutable _eval2:Eval2 = evalfun  //fun (envs:Zenv list) (ts:Term list) (os:Term list) -> [],[],[]
    do 
        printfn "state: %A" state
    

    
    member self.Clone with get() =
                        let state1= { state with 
                                        penvs   = self ::state.penvs       //paren envs
                                        nsStack = state.nsStack.Head ::state.nsStack
                                        kvStack = state.kvStack.Head ::state.kvStack
                                    }
                        let newenv = new  Zenv(self.Name, state1, self.Eval2, state.lgr)
                        // state <- { state with cenvs=newenv ::state.cenvs; } //child envs
                        newenv.Eval2 <- self.Eval2
                        
                        newenv
     member self.Name    with get() = name
    member self.State    with get() = state
    // member self.Execute with get() = engine 
    member self.Eval2
        with get()       = _eval2
        and  set(value)  = _eval2 <- value
    member self.LogLevel 
        with get() = state.LOGLEVEL
        and  set v = state.LOGLEVEL <- v
    member self.ErrLevel 
        with get() = state.ERRLEVEL
        and  set v = state.ERRLEVEL <- v
    member self.StepWait 
        with get() = state.STEPWAIT
        and  set v = state.STEPWAIT <- v
    member self.ReplLoop 
        with get() = state.REPLLOOP
        and  set v = state.REPLLOOP <- v

    member self.SetLogger (lgr:string->string->string->unit) = 
       state.lgr <- lgr
       ()

    member inline self.log1 lvl fmt       = if lvl<self.LogLevel then () else elog1 self.Name  "_" lvl fmt
    member inline self.log2 lvl fmt x     = if lvl<self.LogLevel then () else elog2 self.Name  "_" lvl fmt x
    member inline self.log3 lvl fmt x y   = if lvl<self.LogLevel then () else elog3 self.Name  "_" lvl fmt x y
    member inline self.log4 lvl fmt x y z = if lvl<self.LogLevel then () else elog4 self.Name  "_" lvl fmt x y z
    member inline self.log6 lvl fmt x y z z1 z2 = if lvl<self.LogLevel then () else elog6 self.Name  "_" lvl fmt x y z z1 z2
    member self.nslist ()=
        // glog1 lvl "\n==NS=DICTIONARY=====\n"
        // for kv in nsstore do
        //     glog3 lvl "%s -> %O\n" kv.Key kv.Value
        // glog2 lvl "\ntotal %d\n" nsstore.Count
        // glog1 lvl "\n====================\n"
        ()
    member self.lookup (nm:string) :Term=
        let err0 (errlvl, errcode, errtxt) =
            glog2 10 "\n\nE: %s\n\n" errtxt
            E errtxt
        let vo = state.nsStack.Head.TryFind nm
        if vo.IsSome then vo.Value else err0 (ERR_NAME_MISSING_OR_COND_NOT_OK nm)
    member self.nsdrop (nm:string) :Term=
        let vo = state.nsStack.Head.TryFind nm        
        if vo.IsSome then state.nsStack <- state.nsStack.Head.Remove nm ::state.nsStack.Tail; vo.Value else E (sprintf "ERR: Drop Name Not Found. %A" nm)
    member self.assign (nm:string) (tm:Term) :Term=
        let vo = state.nsStack.Head.TryFind nm
        if vo.IsNone then state.nsStack <- state.nsStack.Head.Add(nm, tm) ::state.nsStack.Tail; tm else E (sprintf "ERR: Name Exists. (Use assign2) %A" nm)
    member self.assign2 (nm:string) (tm:Term) : Term =
        state.nsStack <- state.nsStack.Head.Add(nm, tm) ::state.nsStack.Tail
        tm

    member self.kvset (k:string) (v) =
        state.kvStack <- state.kvStack.Head.Add(k, v) ::state.kvStack.Tail
        ()
    member self.kvget (k:string)=
        state.kvStack.Head.Item k
    member self.Item 
        with get k      = self.kvget k
        and  set k v    = self.kvset k v
    member self.kvlst()=
        for kv in state.kvStack.Head do
            printfn "kv   >%O" kv

    member self.HasDbgInfo() =
        dbginfo
    member self.SetDbgInfo txt =
        dbginfo <- txt
        ()
    member self.dummy () = 
        // xxx.Peek()
        ()
and Eval2=Zenv list->Term list->Term list->Zenv list*Term list*Term list
// [<StructuralEquality>]
// [<Struct>]
and LTerm= int*Term
and Term =
    | A     of string * string
    | G     of string * (Term list)
    | B     of string                  // BuildIns ops / term property access OPERATIONS
    | O     of obj

    | W     of string
    | V     of string
    | Y     of string                  // x:y0 for example
    | M     of string
    | S     of string

    | K     of string                  // obj  property access
    | H     of string                  // hold TOS as name
    | R     of string                  // read name and put on stack else put E `NOT FOUND`

    | L     of Term list               //literal array
    | X     of Term list               //executable array
    | U     of Term list               //union of terms
    | J     of Term list               //join of terms
    | JJ    of Term list               //join of terms per starting tip (bitwise)

    // | N of Number
    | N     of string
    | D     of double
    | I     of int
    // | C of Term' list               //function call invoke pa
    | F     of (obj[] -> obj)          //function 
    | E     of string                  //error
    | Zzzz  of int list
    | P     of Path'
    | Q     of Quad'
    | Tvvv  of Tval'
    | Ts    of seq<Term>
    | Tz    of seq<Tp>

    | UU    of string

    | LT    of int*Term
    | CPTF  of (int*Term)*(  Zenv list->Term list->Term list-> Zenv list*Term list*Term list   )


    static member inline New (o:obj)=
        match o with
        | :? Term           as o  ->     o
        | :? (Term list)    as o  ->  L  o
        | :? (seq<Term>)    as o  ->  Ts o
        | :? int            as o  ->  I  o
        | :? string         as o  ->  S  o
        | :? ( obj[]->obj ) as f  ->  F  f 
        | :? ( obj[]->unit) as f  ->  F  ( fun ( pa:obj[] ) ->  f(pa); null ) 
        | :? (  unit->unit) as f  ->  F  ( fun ( pa:obj[] ) ->  f(  ); null )               
        | :? (  unit->obj ) as f  ->  F  ( fun ( pa:obj[] ) ->  f(  )       )

        | :? obj   as res    when 0<res.GetType().GetMember("Invoke").Length  ->
                        F (fun ( args:obj[] ) -> 
                                let res0=res.GetType().InvokeMember("Invoke", BindingFlags.InvokeMethod, null, res, args ) 
                                res0 )
 
        | _ ->    O o

    // interface System.Collections.Generic.IEnumerable<Term> with
    member self.GetEnumerator() = //: System.Collections.Generic.IEnumerator<Term>= 
        match self with
            | Ts ss                          ->       ss.GetEnumerator()
            |  O ss  when (ss:? IEnumerable) ->
                            ((seq{for s in (ss :?> IEnumerable) do yield Term.New s; })).GetEnumerator() 
            | _                            -> 
                            ((seq {yield self;}                               )).GetEnumerator()
        // ((seq {yield I 1;}):>IEnumerable).GetEnumerator()
    member self.ToRawString()=
        match self with
            | A(x,y)    ->  y
            | W w       ->  w   
            | S s       ->  s 
            | M m       ->  m
            | N n       ->  n
            | V v       ->  v
            | G(_, g)   ->  List.fold (fun (s:string) (x:Term) -> s + x.ToRawString()) "" g
            | _         ->
                            sprintf "ToRawString() not Implemented for term %A" self

    override self.ToString()=
        match self with
            | G(x,y)    ->  sprintf "G%s[ ...... ]" x //(sprintf "%O" y)
            | A(x,y)    ->  sprintf "A%s|%s" x y
            | W w       ->  sprintf "W|%s" "``"   //w
            | S s       ->  sprintf "S|`%s`" s 
            | M m       ->  sprintf "M|%s" m
            | N n       ->  sprintf "N|%s" n
            | V v       ->  sprintf "V|%s" v
            | Y y       ->  sprintf "Y|%s" y
            | K x       ->  sprintf ".|%s" x
            | R x       ->  sprintf "?|%s" x
            | H x       ->  sprintf "#|%s" x
            | O o       ->  sprintf "O|%O" o
            | L o       ->  sprintf "L|%O" o
            | P o       ->  sprintf "P|%s%s%s[%O]%s" o.n0 o.n1 o.op o.ys o.ms
            | X o       ->  sprintf "X|%O" o
            | U o       ->  sprintf "U|%O" o
            | J o       ->  sprintf "J|%O" o
            |Tz o       ->  sprintf "Tz...."
            | Zzzz z     ->
                            sprintf "Tzz..."
                        // if 3<z.Length then
                        //     sprintf "\nZ[%O; %O; %O; %O; .... ]" z.[0] z.[1] z.[2] z.[3]
                        // else
                        //     sprintf "\nZ%O" z
            | Tvvv t    ->  sprintf " T|{ws Lazy}" 
            | CPTF( pt,f)    -> sprintf "CPTF %A" pt 
            | x      -> 
                        sprintf "%A" x    //be carefull may overflow       
                                                //sprintf "%O" x  FAILS UUUUUPPPPPS!!!!!!!!!!!!! STACK OVERFLOW

    member self.typ       //very IMPORTANT each prop access drops the tos
        with get() =
            [self]
    member self.dup       //very IMPORTANT each prop access drops the tos
        with get() =
            [self;self]
    member inline self.len
        with get() =
            match self with
                // | O o   ->    o
                | Zzzz x   ->  [ I x.Length ]
                | S x   ->  [ I x.Length ]
                | L x   ->  [ I x.Length ]
                | X x   ->  [ I x.Length ]
                | _     ->    
                            [ E "ERR: OP not supported" ] 

    member inline self.op_dummy (rs:Term list) (os:Term list) = match os with | _ ->  (rs, E "ok" ::os)
    member self.emptyZ       
        with get() = [Zzzz [];self]
    member self.asNum
        with get() =
            match self with
                // | O o   ->    o
                // | S x   ->    x :> obj
                | I x   ->    self
                | D x   ->    self
                | N n   ->  
                            let ok0,n0=Int32.TryParse n
                            let ok1,n1=Double.TryParse n
                            // zlog (ok0,n0,ok1,n1)
                            if ok0 then I n0 else if ok1 then D n1 else E "NaN"
                            // I 43
                | _     ->    
                            printfn "toNum()unhandled Type %A" self
                            E "NaN" 
    member self.asObj
        with get() =
            match self with
                | O o   ->    o
                | S x   ->   box x 
                | I x   ->   box x
                | D x   ->   box x
                | N x   ->  
                              // zlog (self.toNum()) has type Term not Int
                              self.asNum.asObj //IMPORTANT feed again to get correct result
                | F x   ->   box x
                | L x   ->  
                            let y=List.map (fun (z:Term) -> z.asObj ) x                
                            box y 
                | _     ->    
                            printfn "toObj()unhandled Type %A" self
                            null             




    // member self.toObjxxx()     =
    //     match self with
    //         | O o   ->    o
    //         | S x   ->    x :> obj
    //         | I x   ->    x :> obj
    //         | D x   ->    x :> obj
    //         | N x   ->    
    //                       // zlog (self.toNum()) has type Term not Int
    //                       self.toNumxxx().toObjxxx() //IMPORTANT feed again to get correct result
    //         | _     ->    
    //                     printfn "toObj()unhandled Type %A" self
    //                     self  :> obj
    // member self.toNumxxx() :Term'    =
    //     match self with
    //         // | O o   ->    o
    //         // | S x   ->    x :> obj
    //         | I x   ->    self
    //         | D x   ->    self
    //         | N n   ->  
    //                     let ok0,n0=Int32.TryParse n
    //                     let ok1,n1=Double.TryParse n
    //                     zlog (ok0,n0,ok1,n1)
    //                     if ok0 then I n0 else if ok1 then D n1 else E "NaN"
    //                     // I 43
    //         | _     ->    
    //                     printfn "toNum()unhandled Type %A" self
    //                     E "NaN"     

    member self.testMethod0()= I 1234
    member self.testMethod1(id:int)= I (2*id)
    member self.testMethod2( x:Term, y:Term)= ()
    member self.testProp0    = I 5555

    // member self.forEach(code:Term') = 
    //     match self with
    //         | L vs      ->  
    //                         (forEachLoop:?>Term' list->Term'->Term' list) vs code
    //         | _         ->  [self]

and Path' = {op:string; ys:Term list; ms:string ; n0:string; n1:string}
and Tval' = { ws:Lazy<int list list>; } ////walks lists
and Quad' = obj*obj*obj*obj
    
    
and 
    [<StructuredFormatDisplay("{_Print}")>]
    Tp={ bo:Tp option; ws:int list; }
        member  self._Print = "Tp..."
        member self.tip with get () = self.ws.Head
        member self.sip with get () = if self.bo.IsSome then self.bo.Value.tip else 0


// type AAA (s:Term') (p:Term') (o:Term') (h:Term') with
//     member that.s()=
//         zlog "x"
//         ()
// let w=""

let mutable replTerms:Term list=[]
let q= Q (null,null,null,null)
// q.s()  |> zlog

let newPath={op="";ys=[]; ms=""; n0="1"; n1="1";}
let newTval={ws= lazy[[1]]; }
let newTvWs ws= Tvvv{newTval with ws=ws;}
let enumTips (t:Tval') = [for w in t.ws.Value do yield w.Head]

let newTp={bo=None; ws=[];}


let err0 (errlvl, errcode, errtxt) =
    glog2 10 "\n\nE: %s\n\n" errtxt
    E errtxt


let rec printTerms prefix (terms:Term list) =
    let tab="  "
    if 0=terms.Length then printfn "%s[]" prefix // jandle empty terms IMPORTANT
    for t in terms do
        match t with
        | W x      -> printfn "%sW=`%s`" prefix (x.Replace("\n","Lnn").Replace("\r","Lnn"))
        | S x      -> printfn "%sS=`%s`" prefix (x.Replace("\n","Lnn").Replace("\r","Lnn"))
        | A (x,y)  -> printfn "%sT=%A" prefix (x,y)
        | G (x, y) -> 
                        printfn "%sG%s" prefix x
                        printTerms (prefix + tab) y
        | L o   ->    
                        printfn "%s %s" prefix "L"
                        printTerms (prefix + tab) o
        | X o     ->    
                        printfn "%s_%s" prefix "X"
                        printTerms (prefix + tab) o
        | U o       ->    
                        printfn "%sP%s" prefix "U"
                        printTerms (prefix + tab) o
        | J o       ->    
                        printfn "%sP%s" prefix "J"
                        printTerms (prefix + tab) o
        | P z       ->    
                        printfn "%sP%s%s%s|%s" prefix z.n0 z.n1 z.op z.ms
                        printTerms (prefix + tab) z.ys
        | _         ->  
                        printfn "%s%A" prefix t


    

let newEnvState={ 
        level=0
        penvs=[]
        cenvs=[]  //overflows!!!!
        lgr  =(fun z k v -> printfn "%s-%s:envlog> %s" z k v ) 
        desc =""
        src=""
        LOGLEVEL=3
        ERRLEVEL=0
        STEPWAIT=0
        REPLLOOP=false
        nsStack=[Map.empty<string,Term>]
        kvStack=[Map.empty<string,Term>]
        cts =null
        task=null
    }
newEnvState.desc <- "ddd"



type ZGlobal()=
    member x.Prop0 = 3
    member x.Method0 (i) =  i + 3 
    member x.Method1 i j =  i + j 
let zglobal=new ZGlobal()
let getGlobal() = 
    zglobal




let rec sprintfTerms prefix terms =
    let tab="   "
    let sb=new StringBuilder()
    let sadd s=
        sb.AppendLine s |> ignore
        ()
    let saddnnl (s:string) =
        sb.Append(s) |> ignore
        ()
    let sprintfList (o:Term list) =
        if 0< o.Length then 
            sprintfTerms (tab + prefix ) o 
        else 
            ( tab + prefix + "[]\n")


    for t in terms do
        match t with
        | W x      ->   sprintf "%sW|`%s`" prefix (x.Replace("\n","Lnn").Replace("\r","Lnn")) 
                        |> sadd
        | S x      ->   sprintf "%sS|`%s`" prefix (x.Replace("\n","Lnn").Replace("\r","Lnn"))
                        |> sadd
        | A (x,y)  ->   //sprintf "%sA%s|%s" prefix (x,y)
                        sprintf "%sA%s|%s" prefix x y
                        |> sadd
        | G (x, ts) -> 
                        sprintf "%sG%s" prefix x
                        |> sadd
                        sprintfList ts  |> saddnnl
        | L ts   ->    
                        sprintf "%s %s" prefix "L"
                        |> sadd
                        sprintfList ts  |> saddnnl
        | X ts     ->    
                        sprintf "%s_%s" prefix "X"
                        |> sadd
                        sprintfList ts  |> saddnnl
        | U ts       ->    
                        sprintf "%s %s" prefix "U"
                        |> sadd
                        sprintfList ts  |> saddnnl
        | J ts       ->    
                        sprintf "%s %s" prefix "J"
                        |> sadd
                        sprintfList ts  |> saddnnl
        | JJ ts       ->    
                        sprintf "%s%s" prefix "JJ"
                        |> sadd
                        sprintfList ts  |> saddnnl
        | P z       ->    
                        sprintf "%sP%s%s%s|%s" prefix z.n0 z.n1 z.op z.ms
                        |> sadd
                        sprintfList z.ys  |> saddnnl

        // | []        ->
        //                 sprintf "[]" 
        //                 |> sadd
        | _         ->  
                        // printfn "????Term %A" t
                        sprintf "%s%A" prefix t
                        |> sadd

    sb.ToString() //.Replace("\r\n","\n")


// type MyNamespace.Common.Term with  
//     member self.op_dummy111111111111111111111111 (rs:Term list) (os:Term list)=
//         (rs, I 55533 ::os)


let printRawStack (max:int) (xs:Term list) :string=

    let  trim_xs=List.truncate max xs

    let sb=new StringBuilder()

    let sadd (s:string) = sb.Append s; () 
    
    
    if 0=xs.Length then      
        sadd ( sprintf "\n.  %s" "( *empty* )" )
    else 
        for i in [0..trim_xs.Length-1] do
            let s=sprintf ".  %A" xs.[i]
            let s=s.Replace("\n", "Lnn").Replace("\r", "Lrr")
            sadd "\n"  
            if s.Length <64 then sadd s else sadd ( (s.Remove 50) + " ..." )

    if max<xs.Length  then      
        sadd (sprintf "\n.  %s" "..." )        
    sadd "\n"

    sb.ToString()



let rec handleResult (res:obj):Term list =
    match res with
        | :? Term          as res  ->     [   res]
        | :? (Term list)   as res  ->         res
        | :? (seq<Term>)   as res  ->     [Ts res]
        | :? string        as res  ->     [S  res]
        | :? int           as res  ->     [I  res]
        | :? double        as res  ->     [D  res]

        // | :? ITuple        as 

        | :? ( obj[]->obj ) as f         -> [ F f ]
        | :? ( obj[]->unit) as f         -> [ F ( fun ( pa:obj[] ) ->  f(pa); null ) ]

        | :? (  unit->unit) as f         -> [ F ( fun ( pa:obj[] ) ->  f(  ); null ) ]              
        | :? (  unit->obj ) as f         -> [ F ( fun ( pa:obj[] ) ->  f(  )       ) ]

        // | :? ( obj->obj->obj->unit) as f -> [ F ( fun ( pa:obj[] ) ->  f pa.[0] pa.[1] pa.[2] ;null )  ]                  
        // | :? (      obj->obj->unit) as f -> [ F ( fun ( pa:obj[] ) ->  f pa.[0] pa.[1]       ;null )  ]                  
        // | :? (           obj->unit) as f -> [ F ( fun ( pa:obj[] ) ->  f pa.[0]             ;null )  ]                  
        // | :? ( obj->obj->obj->obj) as f  -> [ F ( fun ( pa:obj[] ) ->  f pa.[0] pa.[1] pa.[2] )  ]                  
        // | :? (      obj->obj->obj) as f  -> [ F ( fun ( pa:obj[] ) ->  f pa.[0] pa.[1]       )  ]                 
        // | :? (           obj->obj) as f  -> [ F ( fun ( pa:obj[] ) ->  f pa.[0]             )  ]                 

        ///////////////////////////////////////VERY VERY SPECIAL e.x. List.contains has member Invoke with one or two args    
        | :? obj   as res    when 0<res.GetType().GetMember("Invoke").Length  ->
                        glog2 10 "\n\nSPECIAL CASTING...\n %A\n\n"   (res.GetType().GetMember("Invoke"))      
                        [ F (fun ( args:obj[] ) -> 
                                let res0=res.GetType().InvokeMember("Invoke", BindingFlags.InvokeMethod, null, res, args ) 
                                // glog2 10 "\n\nSPECIAL RESULT %A\n\n" res0
                                // box res0
                                res0
                                ) ]


        | :? obj   as res            -> [ O res ]
        | _                          -> 
                                        //cast to NULL everything else
                                        glog2 10 "CAST TYPE??? %A" res //(res.GetType())
                                        [ O null ]
        // | _                          -> 
        //                                 glog2 10 "CAST TYPE??? %A" res //(res.GetType())
        //                                 [ E "FSHARP CAST ERROR" ] //



let linepos (s:string) (pos:int) =
    let s=s.Substring(0,pos)
    let ss=s.Split('\n')
    let lines=ss.Length
    let ss=Array.truncate (ss.Length-1) ss
    lines, pos + 1 - (Array.fold (fun n (s:string) -> n + s.Length + 1) 0 ss) //line endings count

let dbgme x =
    printfn "~~dbgme~~~~~~~~ %A" x



    