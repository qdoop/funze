module MyNamespace.zscript8tokenizer

#nowarn "20"

open MyNamespace.Global
open MyNamespace.Helper
open MyNamespace.Common

open System
open System.Text

// open MyNamespace.walk7path


let lvl=2
let token8script (env:Zenv) (txt:string) =
    glog1 lvl "\ntoken8script() ...\n"
    // printfn "tokenPathx(`%s`)" (txt.Replace("\n","Lnn").Replace("\r","Lrr").Replace("\t","Ltt"))

    let txtLrrLnn=txt+"\r\n"
    let cs=[for c in txtLrrLnn  do yield c]   //IMPORTANT ADD NEWLINE AT THE END
    let mutable pos=0
    let inline emit (t:string) (s:string) pts =
        // printfn "emit %A" (t, s) 
        let pts=(pos,t,s) ::pts
        pos <- pos + s.Length
        pts

    let inline sss c=
        c.ToString()//.Replace("\n","Lnn").Replace("\r","Lrr").Replace("\t","Ltt")

    let rec nextString00 (t:string) (s:string) (cs: char list)=
        // blocks of the form: " body " where  the t=" and s the body
        match cs with
            | '\\' :: '"' ::rs    ->
                                let s = s + "\\\""
                                nextString00 t s rs
            | '"' ::rs    ->                                
                                (s + "\"", rs)
            | '\n' ::rs   ->
                                printfn "ERROR UNTERMINATED COMMENT end!!!"
                                raise ( ErrorT("nextString00") )
                                (s, cs)
            | c ::rs      ->
                                let s = s + (sss c)
                                nextString00 t s rs
            | _           ->
                                printfn "ERROR UNTERMINATED end!!!"
                                raise ( ErrorT("nextString00") )
                                (s, cs)

    let rec nextString11 (t:string) (s:string) (cs: char list) =
        // blocks of the form: " body " where  the t=" and s the body
        match cs with
            | '"' ::rs    ->
                                (s + "\"", rs)
            | '\n' ::rs        ->
                                printfn "ERROR UNTERMINATED COMMENT end!!!"
                                raise ( ErrorT("nextString11") )
                                (s, cs)
            | c ::rs    ->
                                let s = s + (sss c)
                                nextString11 t s rs
            | _         ->
                                printfn "ERROR UNTERMINATED end!!!"
                                raise ( ErrorT("nextString11") )
                                (s, cs)

    let rec nextString22 (t:string) (s:string) (cs: char list)=
        // blocks of the form: <b{ bb }> where b the type and bb the body
        match cs with
            | '"' :: '"' :: '"' ::rs    ->
                                (s + "\"\"\"", rs)
            | c ::rs    ->
                                let s = s + (sss c)
                                nextString22 t  s rs
            | _         ->
                                printfn "ERROR UNTERMINATED end!!!"
                                raise ( ErrorT("nextString22") )
                                (s, cs)

    let rec nextComment00 (t:string) (s:string) (cs: char list)=
        // blocks of the form: <b{ bb }> where b the type and bb the body
        match cs with
            | '\n' ::rs    ->
                                (s + "\n" , rs)
            | c ::rs    ->
                                let s = s + (sss c)
                                nextComment00 t  s rs
            | _         ->
                                printfn "ERROR UNTERMINATED end!!!"
                                ("ERROR UNTERMINATED", cs)

    let rec nextBlock00 (t:string) (s:string) (cs: char list)=
        // blocks of the form: <t{ s }> where t the type and s the body
        match cs with
            | '}' :: '>' ::rs   ->
                                    (s + "}>", rs)
            | '"' ::rs          ->
                                    let s = s + str_1S
                                    let ss, rs = nextString00 "\"" s rs
                                    nextBlock00 t (s+ss) rs
            | c ::rs            ->
                                    let s = s + (sss c)
                                    nextBlock00 t  s rs
            | _                 ->
                                    printfn "ERROR UNTERMINATED end!!!"
                                    raise ( ErrorT("nextBlock00") )
                                    (s, cs)

    let rec nextWhitespace00 (t:string) (s:string) (cs: char list)=
        // blocks of the form: <t{ s }> where t the type and s the body
        match cs with
            | c ::rs   when not (Char.IsWhiteSpace c) ->
                                (s, cs)
            | c ::rs    ->
                                let s = s + (sss c)
                                nextWhitespace00 t s rs
            | []        ->
                                (s, cs)
            | _         ->
                                printfn "ERROR UNTERMINATED end!!!"
                                raise ( ErrorT("nextWhitespace00") )
                                (s, cs)

    let rec nextNumeric00 (t:string) (s:string) (cs: char list)  =
        // blocks of the form: <t{ s }> where t the type and s the body
        match cs with
            | c ::rs   when Char.IsWhiteSpace c || -1<"{}()[]".IndexOf c ->
                                (s, cs)
            | c ::rs    ->
                                let s = s + (sss c)
                                nextNumeric00 t  s rs
            | _         ->
                                printfn "ERROR UNTERMINATED end!!!"
                                raise ( ErrorT("nextNumeric00") )
                                (s, cs)

    let rec nextVariable00 (t:string) (s:string) (cs: char list)=
        // blocks of the form: <t{ s }> where t the type and s the body
        match cs with
            | c ::rs   when Char.IsWhiteSpace c || -1<"{}()[]\\/,.<>=+-*;".IndexOf c ->
                                (s, cs)
            | c ::rs    ->
                                let s = s + (sss c)
                                nextVariable00 t  s rs
            | _         ->
                                printfn "ERROR UNTERMINATED end!!!"
                                raise ( ErrorT("nextVariable00") )
                                (s, cs)

    let rec nextUri00 (t:string) (s:string) (cs: char list) =
        // blocks of the form: <t{ s }> where t the type and s the body
        match cs with
            | '>' ::rs  ->
                            (s + ">", rs)
            | c ::rs   when Char.IsWhiteSpace c || -1<"{}<\n\r".IndexOf c ->
                            raise ( ErrorT("nextUri00") )
                            (s, cs)
            | c ::rs    ->
                            let s = s + (sss c)
                            nextUri00 t s rs
            | _         ->
                            printfn "ERROR UNTERMINATED end!!!"
                            raise ( ErrorT("nextUri00") )
                            (s, cs)

    let rec nextFsharp00 (st:string list) (t:string) (s:string) (cs: char list) =
        // blocks of the form: <t{ s }> where t the type and s the body
        match st, cs with
            | _, '\'' :: '}' :: '\'' ::rs  ->
                                let s = s + "'}'"
                                nextFsharp00 st t s rs
            | _, '\'' :: '"' :: '\'' ::rs  ->
                                let s = s + "\""
                                nextFsharp00 st t s rs
            | _, '(' :: '*' ::rs  ->
                                    let s = s + "(*"
                                    let rec next0 (s:string) (cs: char list) =
                                        match cs with
                                            | '*' :: ')' ::rs  ->
                                                                let s = s + "*)"
                                                                (s, rs)
                                            | c ::rs           ->
                                                                let s = s + (sss c)
                                                                next0 s rs
                                            | _                 ->
                                                                printfn "ERROR UNTERMINATED end!!!"
                                                                raise ( ErrorT("nextFsharp00") )
                                                                (s, cs)
                                    let s, cs= next0 s rs 
                                    nextFsharp00 st t s cs
            | _, '/' :: '/' ::rs    ->
                                    let s = s + "//"
                                    let ss, rs = nextComment00 "//" s rs
                                    nextFsharp00 st t (s+ss) cs
            | _, '"' :: '"' :: '"' ::rs  ->
                                    let s = s + str_3S
                                    let ss, rs = nextString22 "\"\"\"" s rs
                                    nextFsharp00 st t (s+ss) cs
            | _, '@' :: '"' ::rs  ->
                                    let s = s + str_2S
                                    let ss, rs = nextString11 "@\"" s rs
                                    nextFsharp00 st t (s+ss) cs
            | _, '"' ::rs  ->
                                    let s = s + str_1S
                                    let ss, rs = nextString00 "\"" s rs
                                    nextFsharp00 st t (s+ss) cs
            | "{" ::st, '}' ::rs   ->
                                    let s = s + "}"
                                    nextFsharp00 st t s rs
            |   _   , '{' ::rs     ->
                                    let s = s + "{"
                                    nextFsharp00 ("{"::st) t s rs
            | [], '}' ::rs  ->
                                    (s + "}", rs)
            |  _,  c ::rs   ->
                                    let s = s + (sss c)
                                    nextFsharp00 st t s rs
            | _ , _         ->
                                    printfn "ERROR UNTERMINATED end!!!"
                                    raise ( ErrorT("nextFsharp00") )
                                    (s, cs)


    let rec next (v:string) (cs:char list) (pts:(int*string*string)list) =

        match cs with
            | c ::rs    ->
                                // printfn "c=%A" c 
                                ()
            | _         ->
                                printfn "end!!!"  

        match cs with
            | c ::rs when Char.IsWhiteSpace c   ->
                                                    let t = "W"
                                                    let s = sss c
                                                    let s, rs = nextWhitespace00 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            // | p ::c ::rs when Char.IsDigit c && -1<"+-".IndexOf(p)    ->
            //                                         let t = "N"
            //                                         let s = sss p + sss c
            //                                         let s, rs, pts = nextNumeric00 t s rs
            //                                         let pts = emit t s pts
            //                                         next v rs pts
            | c ::rs when Char.IsDigit c   ->
                                                    let t = "N"
                                                    let s = sss c
                                                    let s, rs = nextNumeric00 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | c ::rs when Char.IsLetter c   ->
                                                    let t = "V"
                                                    let s = sss c
                                                    let s, rs = nextVariable00 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | '#' :: '{' ::rs                   ->
                                                    let t = "#{"
                                                    let s = "#{"
                                                    let s, rs = nextFsharp00 [] t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | '/' :: '/' ::rs                   ->
                                                    let t = "//"
                                                    let s = "//"
                                                    let s, rs = nextComment00 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | '"' :: '"' :: '"' ::rs            ->
                                                    let t = "\"\"\""
                                                    let s = "\"\"\""
                                                    let s, rs = nextString22 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | '@' :: '"' ::rs                   ->
                                                    let t = "@\""
                                                    let s = "@\""
                                                    let s, rs = nextString11 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | '"' ::rs                          ->
                                                    let t = "\""
                                                    let s = "\""
                                                    let s, rs = nextString00 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | '<' :: c :: '{' ::rs              ->
                                                    let t = "<" + sss c + "{"
                                                    let s = "<" + sss c + "{"
                                                    let s, rs = nextBlock00 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | '<':: c ::rs      when  not (Char.IsWhiteSpace c)  ->
                                                    let t = "<"
                                                    let s = "<" + sss c
                                                    let s, rs = nextUri00 t s rs
                                                    let pts = emit t s pts
                                                    next v rs pts
            | c  ::rs when Char.IsSymbol(c)     ->
                                                    let pts=emit  "M" (sss c) pts  // $$$$$$$$$$$
                                                    next v rs pts
            | c  ::rs when Char.IsPunctuation(c)->
                                                    let pts=emit  "M" (sss c) pts
                                                    next v rs pts
            | c  ::rs                           ->  
                                                    raise ( ErrorT("UNMAPPED_SYMBOL:" + (sss c)) )
                                                    let pts=emit  "\nUNMAPPED_SYMBOL" (sss c) pts                                                    
                                                    (v,rs,pts)
            | []                                ->  
                                                    let pts=emit  "EOF" "" pts
                                                    (v,cs,pts)
            |  _                                 ->
                                                    raise ( ErrorT("UNHANDLED MATCH (| _ ->????)") )
                                                    (v,cs,pts)

    let v,rs,pts = next "" cs [] 
    let pts=List.rev pts     //IMPORTANT see 'emit'

    //verify no loss in lexer
    let sb=new StringBuilder()
    for t in pts do 
        let _,_,s=t 
        sb.Append(s)

    let txt0=txtLrrLnn
    let txt1=sb.ToString()

    if txt0=txt1 then printfn "lexingA ok!"
    





    // if (txt0.Length)=txt1.Length then
    //     for i in [0..txt0.Length-1] do
    //         if txt1.[i] = txtLrrLnn.[i] then
    //             ()
    //         else
    //             printfn "lexer lost chars  (NEVER FORGET LrrLnn case) at pos:%A  \r\n%A" i (txt0.Substring(i))
    //             assert false
    //             ()
    // else 
    //     assert false

    glog1 lvl "ntoken8script() ok!\n"

    pts 


