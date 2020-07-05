module MyNamespace.Global
open System

#nowarn "20" 

///////// FUNZE from funge
//Zeven
//open Trinity.Storage
open System.Collections.Concurrent
//let mutable stor:LocalMemoryStorage=null  //Access the GraphEngine Instance
let lvl=3   //default level log  fix it on top of each file

let mutable LOGLEVEL=3

let mutable ERRLEVEL=5
let mutable STEPWAIT=0
let mutable REPLLOOP=false


let str_3S="\"\"\""
let str_2S="@\""
let str_1S="\""

exception ErrorT of string

type ZStream ={ t:DateTime; z:string; k:string; v:string; }
let zlog z =
    printfn "zlog: %O" z
    ()

let mutable GLogger:string ->string ->string -> unit = fun z k v -> printf "%s" v; ();

let streamJSONL=new ConcurrentQueue<string>()
let streamJSONLAppend (s:string) = 
    streamJSONL.Enqueue s


let glog z =
    GLogger "" "" ( sprintf "glog: %O\r\n" z )
    ()

let glog1 lvl  x =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= x
        GLogger  "" ""  txt
        // printf "%s" txt

let glog2 lvl fmt x =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= sprintf fmt x 
        GLogger "" "" txt
        // printf "%s" txt

let glog3 lvl fmt x y =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= sprintf fmt x y 
        GLogger "" ""  txt
        // printf "%s" txt

let glog4 lvl fmt x y z =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= sprintf fmt x y z
        GLogger "" "" txt
        // printf "%s" txt




let elog1 z k lvl  x =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= x
        GLogger z k txt
        // printf "%s" txt

let elog2 z k lvl fmt x =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= sprintf fmt x 
        GLogger z k txt
        // printf "%s" txt

let elog3 z k lvl fmt x y =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= sprintf fmt x y 
        GLogger z k txt
        // printf "%s" txt

let elog4 z k lvl fmt x y z1 =
    if lvl<LOGLEVEL then
        ()
    else
        let txt= sprintf fmt x y z1
        GLogger z k txt
        // printf "%s" txt

let elog6 z k lvl fmt x y z1 z2 z3=
    if lvl<LOGLEVEL then
        ()
    else
        let txt= sprintf fmt x y z1 z2 z3
        GLogger z k txt
        // printf "%s" txt


let glogs o =
    // GLogger <|  o.ToString()
    ()

// let rec glogf o =
//     GLogger <|  o.ToString()
//     fun x ->
//         glogf x



let xlog x =  zlog x; glog x;



//////////////////////////////////////////
let mutable gTripleCount=0

let ppred (x:int) =
   if 100< x && x<999 then 
        x &&& 0x1FF
    else 
        (assert false) 
        -1

let qpred (x:int) =
   if 100< x && x<999 then 
        x ||| 0x200 
    else 
        (assert false) 
        -1

let liter (x:int) =
    0



type System.Int32 with
    member self.ppred with get()= ppred self
    member self.qpred with get()= qpred self



///// Tetx Diktionary/////////////////////////////////////////////////
let gid2txtMap=new System.Collections.Concurrent.ConcurrentDictionary<int,string>()
let txt2gidMap=new System.Collections.Concurrent.ConcurrentDictionary<string,int>()
let mutable lasttgid= 0

let Addtxt2gid (txt:string) = 
    let ok, gid = txt2gidMap.TryGetValue txt
    if ok then 
        gid
    else
        lasttgid <- lasttgid + 1
        let gid=  (0xA000_0000 ||| lasttgid)
        let ok = txt2gidMap.TryAdd( txt, gid )
        let ok = gid2txtMap.TryAdd( gid, txt )
        let ok, gid = txt2gidMap.TryGetValue txt
        assert ok
        gid

let txt2gid (txt:string) :int =
    let ok, gid = txt2gidMap.TryGetValue txt
    if ok then 
        gid
    else
        0xA000_0000 
let gid2txt (gid:int) :string =
    let ok, txt = gid2txtMap.TryGetValue gid
    if ok then 
        txt
        (sprintf "%%%A%%" txt).Replace("\\","\\\\").Replace("\"","\\\"")
    else
        sprintf "%%%d%%" gid



//GLOBAL GIDs URIs

let gid2uriMap=new System.Collections.Concurrent.ConcurrentDictionary<int,string>()
let uri2gidMap=new System.Collections.Concurrent.ConcurrentDictionary<string,int>()
let mutable lastpgid= 200
let mutable lastzgid=1000
let AddNewUri (uri:string) (pred:bool)=
    let ok, gid = uri2gidMap.TryGetValue uri
    if ok then 
        gid
    elif    pred  then
        let p=sprintf "p:%s" uri
        let q=sprintf "q:%s" uri

        let ok, gid = uri2gidMap.TryGetValue p
        if ok then
            gid
        else          
            lastpgid <- lastpgid + 1

            let ok= gid2uriMap.TryAdd( lastpgid.ppred, p )
            assert ok
            let ok= gid2uriMap.TryAdd( lastpgid.qpred, q )
            assert ok

            let ok= uri2gidMap.TryAdd( p, lastpgid.ppred )
            assert ok
            let ok= uri2gidMap.TryAdd( q, lastpgid.qpred )
            assert ok

            let ok, gid = uri2gidMap.TryGetValue p
            assert ok
            // zlog (gid, p, "addnewuri")
            gid

    else
        let z=if -1< uri.IndexOf(":") then sprintf "z:%s" uri else uri

        let ok, gid = uri2gidMap.TryGetValue z
        if ok then
            gid
        else
            lastzgid <- lastzgid + 1

            let ok= gid2uriMap.TryAdd( lastzgid, z )
            // assert ok
            let ok= uri2gidMap.TryAdd( z, lastzgid )
            // assert ok
            let ok, gid = uri2gidMap.TryGetValue z
            assert ok

            // zlog (gid, z, "addnewuri")
            gid

    

let gid2uri (gid:int)  = //(uri:string) =

    let ok, uri = gid2uriMap.TryGetValue gid

    if ok then //200<gid && gid<400 || 700<gid && gid<99_999 then
        // assert ok 
        uri
    elif  100<gid && gid<999  then

        if gid=(0x1FF &&& gid) then
            sprintf "p:%d" (0x1FF &&& gid)
        else
            sprintf "q:%d" (0x1FF &&& gid)

        // let ok= gid2uriMap.TryAdd( gid, sprintf "f:%d" gid)
        // assert ok
        // let ok= gid2uriMap.TryAdd( gid+50, sprintf "f:%d" (gid+50))
        // assert ok
        // let ok, uri = gid2uriMap.TryGetValue gid
        // assert ok
        // uri

    elif 100_000<=gid && gid < 90_000_000 then

        sprintf "x:%d" gid

        // let ok= gid2uriMap.TryAdd( gid, sprintf "z:%d" gid)
        // assert ok
        // let ok, uri = gid2uriMap.TryGetValue gid
        // assert ok
        // uri
    // elif      200<=gid && gid < 400 && ""<>uri then
    //     let ok= gid2uriMap.TryAdd( gid    , sprintf "f:%s" uri)
    //     assert ok
    //     let ok= gid2uriMap.TryAdd( gid+200, sprintf "f:%s" uri)
    //     assert ok
    //     let ok, uri = gid2uriMap.TryGetValue gid
    //     assert ok
    //     uri
    elif 0x8000_0000=(0xF000_0000 &&& gid) then    sprintf "%%:%d"   (0x0FFF_FFFF &&& gid)
    elif 0x9000_0000=(0xF000_0000 &&& gid) then    sprintf "%%:%d" (-(0x0FFF_FFFF &&& gid))
    
    elif 0xA000_0000=(0xF000_0000 &&& gid) then    gid2txt gid //sprintf "%%%d%%" (0x0FFF_FFFF &&& gid)
    elif 0xB000_0000=(0xF000_0000 &&& gid) then    sprintf "_:%d"   (0x0FFF_FFFF &&& gid)

    elif 0xC000_0000=(0xF000_0000 &&& gid) then    sprintf "_C:%d"   (0x0FFF_FFFF &&& gid)
    elif 0xD000_0000=(0xF000_0000 &&& gid) then    sprintf "_D:%d"   (0x0FFF_FFFF &&& gid)

    elif 99=gid then
        glog "gid2uri!!!99"
        sprintf "x:%d" gid

    else
        // assert false
        sprintf "x:exx:%d" gid

let uri2gid (uri:string)  =
    assert (1<uri.Length)
    if "x:"=uri.[0..1] then 
        let gid=int uri.[2..]
        assert (0<>gid)
        gid
    else
        let ok, gid = uri2gidMap.TryGetValue uri
        // assert ok
        if ok then
            gid
        else
            glog uri
            glog "uri2gid!!!99"
            99   



