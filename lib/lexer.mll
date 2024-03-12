{
  open Parser
  let dig_of_char c = int_of_char c - int_of_char '0'
  let diglist_of_string s =
    List.init (String.length s) (fun i -> dig_of_char s.[i])
  let numlist_of_string x s =
    let n = String.length s in
    let rec split acc i =
      if i=n then acc
      else match String.index_from_opt s i ',' with
           | None -> int_of_string (String.sub s i (n-i)) :: acc
           | Some j -> split (int_of_string (String.sub s i (j-i-1)) :: acc) (j+1)
    in List.rev (split [int_of_string x] 0)
}

let lstart = ['a'-'e' 'g'-'k' 'm'-'r' 't'-'z' ]
let letter = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let key = letter+
let word = letter*
let label = lstart word

let ndigit = ['1'-'9']
let digit = ['0'-'9']
let nint = ndigit digit*

let frac = '.' digit*
let sign = ['-' '+']
let exp = ['e' 'E'] sign? digit+
let float = sign? digit* frac? exp?
let pos = float ',' float

let blank = [ ' ' '\r' '\t' '\n' ]

rule token = parse
  | blank                                  { token lexbuf }
  | '0'                                    { NIL }
  | 'f'                                    { FGT }
  | 'l'                                    { LFT }
  | 's'                                    { SERIES }
  | '|'                                    { PAR }
  | '('                                    { LPAR }
  | ')'                                    { RPAR }
  | '<'                                    { LT }
  | '>'                                    { GT }
  | '.'                                    { DOT }
  | ','                                    { COMMA }
  | '''                                    { QUOTE }
  | ';'                                    { SEMI }
  | '*'                                    { STAR }
  (* cycles&permutations: at least two elements, if comma then arbitrary ints, otherwise digits *)
  | '(' (ndigit ndigit+ as s) ')'          { PRM (Perm.of_cycle (diglist_of_string s)) }
  | '(' (nint as x) ((',' nint)+ as q) ')' { PRM (Perm.of_cycle (numlist_of_string x q)) }
  | '[' (ndigit ndigit+ as s) ']'          { PRM (Perm.of_list (diglist_of_string s)) }
  | '[' (nint as x) ((',' nint)+ as q) ']' { PRM (Perm.of_list (numlist_of_string x q)) }
  (* injections: any number of elements, if only one large int, then have it start with a 0 *)
  | '{' (ndigit* as s) '}'                 { INJ (Inj.of_list (diglist_of_string s)) }
  | "{0" (nint as x)((',' nint)* as q) '}' { INJ (Inj.of_list (numlist_of_string x q)) }
  | '#' (digit+ as x)                      { FIX (int_of_string x) }
  | '-'                                    { LABEL ""}
  | label as s
  | '\\' (word as s)                       { LABEL s }
  | "pos=" (pos as p)
  | "pos=(" (pos as p) ')'                 { KEYVAL ("pos",p) }
  | "shift=" (pos as v)
  | "shift=(" (pos as v) ')'               { KEYVAL ("shift",v) }
  | "radius=" (float as x)                 { KEYVAL ("radius",x) }
  | "scale=" (float as x)                  { KEYVAL ("scale",x) }
  | (key as k) '=' (word as v)             { KEYVAL (k,v) }
  | eof                                    { EOF }
  | _ as c                                 { Printf.kprintf failwith "lexing error near `%c'" c }
