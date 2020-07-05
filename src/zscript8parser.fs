module MyNamespace.zscript8parser

#nowarn "20"

open MyNamespace.Global
open MyNamespace.Helper
open MyNamespace.Common

open System
open System.Text

open MyNamespace.zscript8cptf


let lvl=2
let parse8script (pts:(int*string*string) list) : Term list =
    glog1 lvl "\nparse8script() ...\n"

    let mutable terms=[]
    let emit (t:int*Term)=
        terms <- t ::terms
    
    let err (s)=
        raise ( ErrorT(s) )

    let removeEscapes (t:string) (s:string) :string=
        let sb=new StringBuilder()
        let rec next (cs:char list) =
            match cs with
                | '\\' :: '\\'::rs when t="\""-> 
                                        sb.Append "\\"; next rs
                | '\\' :: 'r' ::rs  ->  sb.Append "\r"; next rs
                | '\\' :: 'n' ::rs  ->  sb.Append "\n"; next rs
                | '\\' :: 't' ::rs  ->  sb.Append "\t"; next rs
                | c ::rs            ->  sb.Append c   ; next rs
                | []                ->  sb
                | _                 ->  sb

        next [for c in s  do yield c]
        // printfn "~~~~~~~~~%A"  (sb.ToString())
        sb.ToString()
 
    for pt in pts do
        match pt with
            |(p,"W", w)   ->  emit( p,W w )
            |(p,"V", v)   ->  emit( p,V v )             
            |(p,"M", m)   ->  emit( p,M m )
            |(p,"N", s)   ->  emit( p,N s )
            |(p,"\"\"\"", s) ->   
                            let s=s.Substring(3,s.Length-6)
                            emit( p,S s )
            |(p,"\"", s)  ->  
                            let s=s.Substring(1,s.Length-2) 
                            let s0=removeEscapes "\"" s
                            emit( p,S s0 )
            |(p,"@\"", s) ->  
                            let s=s.Substring(2,s.Length-3) 
                            // let s=removeEscapes "@\"" s
                            emit( p,S s )
            |(p,"//", s)  ->  emit( p,W s )
            |(p,"/*", s)  ->  emit( p,W s )
            |(p,"<", s)   ->  emit( p,UU s )
            |(p,"#{", s)  ->  
                            let s=s.Substring(2,s.Length-3)
                            emit( p,A ("#",s) )
            |(p,"<p{", s) ->  
                            let s=s.Substring(3,s.Length-5)
                            emit( p,A ("path",s) )
            |(p,"<r{", s) ->  
                            let s=s.Substring(3,s.Length-5)
                            emit( p,A ("rdf",s) )
            |(p,"<s{", s) ->  
                            let s=s.Substring(3,s.Length-5)
                            emit( p,A ("sparql",s) )
            |(p,"<j{", s) ->  
                            let s=s.Substring(3,s.Length-5)
                            emit( p,A ("json5",s) )
            |(p,"<x{", s) ->  
                            let s=s.Substring(3,s.Length-5)
                            emit( p,A ("xml",s) )
            |(p,"EOF", s) ->  emit( p,A ("eof",s) )                                         
            |(p,t,s)      ->  
                            printfn "\n\n================Unmapped Token %A" (t,s)
                            err(sprintf "Unmapped Token at (pos=%A) %A" p (t,s) )
                            emit( p,A (t,s) ) 

    // printTerms "----" terms

    printfn "+++++++++++++++++++++GROUPING+++++++++++++++++++++++"
    let rec group (lev:int) (mode:string) (gs:Term list) (ts:(int*Term) list): Term list * (int*Term) list=
        
        match ts with
            | (p,W w)::rs   ->  // printfn "%d %A <<<< %A\n"lev mode (W ".....")
                            glog4 lvl "%d %A <<<< %A\n"lev mode (W ".....")
            | (p,x)::rs     ->  // printfn "%d %A <<<< %A\n"lev mode x
                            glog4 lvl "%d %A <<<< %A\n"lev mode x          
            | []        ->  glog1 lvl "----oops!!! [] \n"

        // printfn "%A" gs
        match ts with
            // | W w0 :: W w1 ::rs     ->  
            //                             let rs = W (w0+w1) :: rs
            //                             group lev mode gs rs
            |  (p,M ".")::(_,V v) ::rs ->
                                                let ts = (p,K v) ::rs
                                                group lev mode gs ts

            |  (p,M m)::(_,V v) ::rs when List.contains m [".";"?"; "$"; ":"] ->
                                                let ts = (p,V (m+v)) ::rs
                                                group lev mode gs ts

            |  (p,V v)::(_,M m) ::rs when List.contains m ["?"; "&"; ":"] ->
                                                let ts = (p,V (m+v)) ::rs
                                                group lev mode gs ts

            |  (p,M "]") ::rs when -1<mode.IndexOf("[") ->  gs,rs
            |  (p,M "}") ::rs when -1<mode.IndexOf("{") ->  gs,rs
            |  (p,M ")") ::rs when -1<mode.IndexOf("(") ->  gs,rs

            |  (p,M m0)::(_,M m) ::rs when -1<"[{(".IndexOf m && List.contains m0 [".";"?";"'"] ->
                                        let gs0,rs=group (lev+1) (m0+m) [] rs
                                        let gs= pcollect mode (p,G (m0+m, List.rev gs0))  ::gs
                                        group lev mode gs rs

            |  (p,M m) ::rs when -1<"[{(".IndexOf m ->
                                        let gs0,rs=group (lev+1) m [] rs
                                        let gs= pcollect mode (p,G (m, List.rev gs0))  ::gs
                                        group lev mode gs rs

            |  (p,M m) ::rs when -1<"]})".IndexOf m ->
                                        err("")
                                        printfn "UNBALANCED GROUPS\n%A" ts
                                        gs,ts 


            | (p,W w) ::rs          ->  //ignore white space
                                        group lev mode gs rs

            |  t ::rs               ->  
                                        let gs= pcollect mode t ::gs    
                                        group lev mode gs rs
            | []                    ->  
                                        gs,ts 

            | _                     ->  
                                        printfn "ts===========\n%A" ts
                                        err("")
                                        gs,ts 


    let gs, terms= group 0 "" [] (List.rev terms)

    //printTerms "++++" terms
    List.rev gs



