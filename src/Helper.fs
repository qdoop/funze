module MyNamespace.Helper
open System

#nowarn "20"

open MyNamespace.Global

open System
open System.IO
open System.Text
open System.Xml
open System.Net
open System.Threading
open System.Runtime.Serialization

open System.Collections.Generic
open System.Xml.Schema
open System.Xml.Serialization

open System.Runtime.CompilerServices  //IMPORTANT FOR EXTENSIONS


// let zlog z = printfn "zlog: %A" z; ()

let tracedfile = @"C:\zzWORK_ESP32\FSHARP\fz0demo\rdf0dataset.fs"
let zztracePrepare offset fpath =
    let line0:System.String = System.IO.File.ReadAllText( fpath )
    let lines = line0.Split [|'\n'|]// fails in netstandard line0.Split("\r\n")
    // let lines = List.map (fun (x:string) -> x.Trim()) 
    fun lno ->  
        let lno=int lno; 
        // printfn "trace(%d,1): %s" lno lines.[lno-2]
        sprintf  "%s(%d,0):\nTRACE: %s" ".\\rdf0dataset.fs" lno lines.[lno-offset]

let tracefun  = zztracePrepare 2 tracedfile
let tracefun3 = zztracePrepare 3 tracedfile
let zztrace = fun (line:string) ->
                printfn "%s" (tracefun line)
let zztrace3 = fun (line:string) ->
                printfn "%s" (tracefun3 line)

let zztracex = fun (line:string) ->
                printfn " NOT IMPLEMENTED %s" (tracefun line)
                // raise (Exception "SEE ABOVE")
                Thread.Sleep 5000

let zztrace3x = fun (line:string) ->
                printfn " NOT IMPLEMENTED %s" (tracefun line)
                // raise (Exception "SEE ABOVE")
                Thread.Sleep 5000
// zztrace __LINE__



let trcfiles=new System.Collections.Generic.Dictionary<string,string[]>()
let rec ztrc (offset:int) (srcname:string) (lineno:string) =

    if true then
        ()
    elif trcfiles.ContainsKey srcname then
        let pos=int lineno
        let line = trcfiles.[srcname].[pos-offset]
        let filelink = Path.Combine [| __SOURCE_DIRECTORY__;  srcname |]
        let filelink = ".\\" + srcname
        printfn "%s(%s,0)\nTRACE: %s" filelink lineno line
        ()
    else
        // let lines:System.String = System.IO.File.ReadAllText( srcpath )
        // let fpath= __SOURCE_DIRECTORY__ 
        // let fpath= fpath.Replace("fz0demo","fz0wrap") + srcpath
        let fpath= Path.Combine [| __SOURCE_DIRECTORY__;   srcname |]
        // zlog fpath
        let lines:System.String = System.IO.File.ReadAllText( fpath )
        let lines = lines.Split [|'\n'|]
        trcfiles.Add(srcname, lines)
        if trcfiles.ContainsKey srcname then 
            ztrc offset srcname lineno
        ()
    ()

let ztrcd (offset:int) (srcpath:string) (lineno:string) =
    printfn "***********OOPPS!!!************"
    ztrc offset srcpath lineno
    Thread.Sleep 5000
    ()

let ztrcx (offset:int) (srcpath:string) (lineno:string) =
    printfn "***********EXCEPTIONAL************"
    ztrc offset srcpath lineno

    Thread.Sleep 2000000





// [<Extension>]

type xxqq= int*int*int*string



// type Tuple(Int32*P,'O,'G> with
// [<Extension>]
// type 'T1,'T2,'T3,'T4> with
//     member t.s = t.Item1
//     member t.p = t.Item2
//     member t.o = t.Item3
//     member t.g = t.Item4

// type System.Tuple<'T1,'T2,'T3,'T4> with
//     member inline t.Sss = "dddd"
// [<Extension>]
// type System.Tuple<'a> with //'T1,'T2,'T3,'T4> with
//     [<Extension>]
//     static member inline Sss() = "xxx"

// type ttt=Tuple<'T1,'T2,'T3,'T4>
// 
// [<Extension>]
// module EnumerableExtensions =
//     [<CompiledName("OutputAll"); Extension>]
//     type System.Collections.Generic.IEnumerable<'T> with 
//         member x.OutputAll (this:seq<'T>) = 
//             for x in this do 
//                 System.Console.WriteLine (box x)

//     [<CompiledName("Ss"); Extension>]
//     type System.Tuple<'T1,'T2,'T3,'T4> with
//         member inline t.Ss(this:(System.Tuple<'T1,'T2,'T3,'T4>)) = "1234"
//     member t.p = t.Item2
//     member t.o = t.Item3
//     member t.g = t.Item4

// type (int*int*int*string)  with
//     member t.s = 
//         match t with
// //             |(s,p,o,g)  ->  s


// type struct(int*int*int*string) with
//     member t.dd() = "sss"



// [<Extension>]
// type System.Tuple<'a,'b,'c,'d> with
//     [<Extension>]
//     member  inline t.Ss() = "1234"



let inline fx2cid (x:int) (y:int) :int64 = ( ( (int64 x)<<<32 ) ||| ( 0x0000_0000_FFFF_FFFFL &&& (int64 y) ) )
let inline cid2fx (cid:int64) :(int *int) = ( int (cid >>>32), int (cid &&& 0xFFFF_FFFFL) )

let cidfmt (cid:int64) :string = let c = cid2fx cid in sprintf "%09d-%0d" (fst c) (snd c)
let cidlog (cid:int64) :unit   = let c = cid2fx cid in printfn "cid_%09d-%0d" (fst c) (snd c)

//////////8
let xx2cid (p:uint32) (x:uint32) :int64=  int64  ( ((uint64 p)<<<32)|||(uint64 x) )
let cid2xx (cid:int64) :uint32*uint32=  uint32 (((uint64 cid) &&& 0xFFFF_FFFF_0000_0000UL)>>>32) , uint32 (((uint64 cid) &&& 0x0000_0000_FFFF_FFFFUL)>>>0)

type System.Int64 with
    member cid.ppred = let f,x = cid2fx cid in if (100<f && (f= (0x1ff &&& f))) then f else 0
    member cid.qpred = let f,x = cid2fx cid in if (f<999 && (f= (0x200 ||| f))) then f else 0
    member cid.x = let f,x = cid2fx cid in x
    member cid.z = let f,x = cid2fx cid in x

    member cid.hi = fst( cid2xx cid)
    member cid.lo = snd( cid2xx cid)

type System.String with
    member s.gid = 1000 + (int s)

type System.Int32 with
    member i.uri = sprintf "x:%d" i



//can have an extra V to store values
//size can be 24 or droped to 16 bytes
[<Struct>]
type Qig(s1:int, p1:int, o1:int, v1:int, g1:string) = 
    member q.s = s1
    member q.p = p1
    member q.o = o1
    member q.v = v1
    member q.g = g1

    
//size 32 bytes    
[<Struct>]
type Qtg(s1:string, p1:string, o1:string, g1:string) =
    member q.s = s1
    member q.p = p1
    member q.o = o1
    member q.g = g1

let qi (s:int) (p:int) (o:int) (g:string) = 
    Qig(s, p, o, 0, g)
let qv (s:int) (p:int) (o:int) (v:int) (g:string) = 
    Qig(s, p, o, v, g) 
let qt (s:string) (p:string) (o:string) (g:string) = 
    Qtg(s, p, o, g)

// let qi (s:int) (p:int) (o:int) (g:string) =
//     (s,p,o,g)
// let qt (s:string) (p:string) (o:string) (g:string) =
//     (s,p,o,g)






