module MyNamespace.BuildIns

#nowarn "20"

open MyNamespace.Global
open MyNamespace.Helper
open MyNamespace.Common
open MyNamespace.ErrorTexts


type BOps() =
    static member inline op_add (rs:Term list) (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 + n0                in (rs, I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 + n0                in (rs, D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) + (float n0) in (rs, N (sprintf "%A" x) ::ps)
            | I n0::D n1 ::ps  -> let x=(float n1) + (float n0) in (rs, D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) + (float n0) in (rs, D x ::ps)

            | S n0::S n1 ::ps  -> let x= n1 + n0 in (rs, S x ::ps)    //string addition        
            | _     ->    
                        (rs, E "ERR: OP not supported" ::os)
    static member inline op_sub (rs:Term list) (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 - n0                in (rs, I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 - n0                in (rs, D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) - (float n0) in (rs, N (sprintf "%A" x) ::ps)
            | I n0::D n1 ::ps  -> let x=(float n1) - (float n0) in (rs, D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) - (float n0) in (rs, D x ::ps)
            | _     ->    
                        (rs, E "ERR: OP not supported." ::os)
    static member inline op_mlt (rs:Term list) (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 * n0                in (rs, I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 * n0                in (rs, D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) * (float n0) in (rs, N (sprintf "%A" x) ::ps)
            | I n0::D n1 ::ps  -> let x=(float n1) * (float n0) in (rs, D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) * (float n0) in (rs, D x ::ps)
            | _     ->    
                        (rs, E "ERR: OP not supported" ::os)
    static member inline op_div (rs:Term list) (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 / n0                in (rs, I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 / n0                in (rs, D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) / (float n0) in (rs, N (sprintf "%A" x) ::ps)

            | I n0::D n1 ::ps  -> let x=(float n1) / (float n0) in (rs, D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) / (float n0) in (rs, D x ::ps)
            | _     ->    
                        (rs, E "ERR: OP not supported" ::os)
    static member inline op_mod (rs:Term list) (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= (n1 % n0)                in (rs, I x ::ps)
            | D n0::D n1 ::ps  -> let x= (n1 % n0)                in (rs, D x ::ps)
            | N n0::N n1 ::ps  -> let x=((float n1) % (float n0)) in (rs, N (sprintf "%A" x) ::ps)
            | _     ->    
                        (rs, E "ERR: OP not supported" ::os)

    static member inline op_add2 (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 + n0                in ( I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 + n0                in ( D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) + (float n0) in ( N (sprintf "%A" x) ::ps)
            | I n0::D n1 ::ps  -> let x=(float n1) + (float n0) in ( D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) + (float n0) in ( D x ::ps)

            | S n0::S n1 ::ps  -> let x= n1 + n0 in ( S x ::ps)    //string addition        
            | _     ->    
                         E "ERR: OP not supported" ::os
    static member inline op_sub2 (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 - n0                in ( I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 - n0                in ( D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) - (float n0) in ( N (sprintf "%A" x) ::ps)
            | I n0::D n1 ::ps  -> let x=(float n1) - (float n0) in ( D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) - (float n0) in ( D x ::ps)
            | _     ->    
                         E "ERR: OP not supported." ::os
    static member inline op_mlt2 (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 * n0                in ( I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 * n0                in ( D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) * (float n0) in ( N (sprintf "%A" x) ::ps)
            | I n0::D n1 ::ps  -> let x=(float n1) * (float n0) in ( D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) * (float n0) in ( D x ::ps)
            | _     ->    
                        E "ERR: OP not supported" ::os
    static member inline op_div2 (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= n1 / n0                in ( I x ::ps)
            | D n0::D n1 ::ps  -> let x= n1 / n0                in ( D x ::ps)
            | N n0::N n1 ::ps  -> let x=(float n1) / (float n0) in ( N (sprintf "%A" x) ::ps)

            | I n0::D n1 ::ps  -> let x=(float n1) / (float n0) in ( D x ::ps)
            | D n0::I n1 ::ps  -> let x=(float n1) / (float n0) in ( D x ::ps)
            | _     ->    
                        E "ERR: OP not supported" ::os
    static member inline op_mod2 (os:Term list)=
        match os with
            | I n0::I n1 ::ps  -> let x= (n1 % n0)                in ( I x ::ps)
            | D n0::D n1 ::ps  -> let x= (n1 % n0)                in ( D x ::ps)
            | N n0::N n1 ::ps  -> let x=((float n1) % (float n0)) in ( N (sprintf "%A" x) ::ps)
            | _     ->    
                        E "ERR: OP not supported" ::os