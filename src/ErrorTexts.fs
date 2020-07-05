module MyNamespace.ErrorTexts



type ERL =
    | OK =0
    | MSG=1
    | INF=2
    | WRN=3
    | ERR=5
    | CRI=7  

let ERR_NONE                          x = ERL.OK,  101, sprintf "ok" 
let ERR_UNKNOWN                       x = ERL.ERR, 101, sprintf "ERR_UNKNOWN %A" x
let ERR_UNHANDLED_CASE                x = ERL.ERR, 101, sprintf "ERR_UNHANDLED_CASE %A" x


let ERR_OP_CONDITIONS_NOTOK           x = ERL.ERR, 101, sprintf "ERR_OP_CONDITIONS_NOTOK %A" x

let ERR_GENERIC_TXT                   x = ERL.ERR, 101, sprintf "ERR_GENERIC_TXT %A" x

let ERR_NAME_LOOKUP_FAIL              x = ERL.ERR, 101, sprintf "ERR_NAME_LOOKUP_FAIL %A" x
let ERR_NAME_MISSING_OR_COND_NOT_OK   x = ERL.ERR, 101, sprintf"ERR_NAME_NOT_FOUND_OR_COND_NOT_OK %A" x

let ERR_RESULT_FORMAT_NOTOK           x = ERL.ERR, 101, sprintf "ERR_RESULT_FORMAT_NOTOK %A" x


let ERR_TERM_OPERATOR_EXCEPTION       x = ERL.ERR, 101, sprintf "ERR_TERM_OPERATOR_EXCEPTION %A" x
let ERR_TERM_OPERATOR_NOTFOUND        x = ERL.ERR, 101, sprintf "ERR_TERM_OPERATOR_NOTFOUND %A" x

let ERR_OBJ_ACCESS_EXCEPTION          x = ERL.ERR, 101, sprintf "ERR_OBJ_ACCESS_EXCEPTION %A" x
let ERR_OBJ_MEMBER_NOTFOUND           x = ERL.ERR, 101, sprintf "ERR_OBJ_MEMBER_NOTFOUND %A" x

let ERR_UNBALANCED_GROUP               x = ERL.ERR, 101, sprintf "ERR_UNBALANCED_GROUP %A" x
