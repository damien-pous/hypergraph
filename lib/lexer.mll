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
  let keyval k v = KEYVAL(Info.kv k v)
  let p2_of_strings x y =
    Gg.P2.v (float_of_string x) (float_of_string y)
  let box2_of_strings x y x' y'=
    Gg.Box2.v (p2_of_strings x y) (p2_of_strings x' y')
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
let skip = [^ ';']* ';'

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
  | '#'                                    { SHARP }
  | '#' (digit+ as x)                      { FIX (int_of_string x) }
  | label as s
  | '-' (word as s)                        { LABEL s }
  | "pos=" (pos as p)
  | "pos=(" (pos as p) ')'                 { keyval "pos" p }
  | "shift=" (pos as v)
  | "shift=(" (pos as v) ')'               { keyval "shift" v }
  | "radius=" (float as x)                 { keyval "radius" x }
  | "scale=" (float as x)                  { keyval "scale" x }
  | (key as k) '=' (word as v)             { keyval k v }
  | eof                                    { EOF }
  | _ as c                                 { Printf.kprintf failwith "lexing error near `%c'" c }


(* for extracting positions in dot files *)
and dotline = parse
  | 's' (nint as i)     { ID (Types.S,int_of_string i) }
  | 'i' (nint as i)     { ID (Types.I,int_of_string i) }
  | 'e' (nint as i)     { ID (Types.E,int_of_string i) }
  | "pos=\""
      (float as x) ','
      (float as y) '"'  { POS(p2_of_strings x y) }
  | "bb=\""
      (float as x1) ','
      (float as y1) ','
      (float as x2) ','
      (float as y2) '"' { BOX(box2_of_strings x1 y1 x2 y2) }
  | ';'                 { SEMI }
  | eof                 { EOF }
  | _                   { dotline lexbuf }
