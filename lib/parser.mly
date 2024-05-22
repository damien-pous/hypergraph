%token LPAR RPAR LT GT COMMA SEMI STAR SERIES DOT CNV HAT SHARP
%token PAR NIL LFT FGT SIM EOF
%token <Types.label> LABEL
%token <Types.perm> PRM
%token <Types.inj> INJ
%token <int> FIX
%token <Info.kv> KEYVAL

%left PAR
%left DOT
%nonassoc LFT FGT PRM INJ CNV

%type <Info.kvl Term.t> sterm
%start sterm

%type <Info.kvl Term.t list> file
%start file



(* for parsing dot files and extracting positions *)
%token <Gg.p2> POS
%token <Gg.box2> BOX
%token <Types.kind*int> ID

%type <Gg.box2*(((Types.kind*int)*Gg.p2) list)> dotlines
%start dotlines

%{
    open Term
    open Flexible
%}

%%

term:
| LPAR; t=term; RPAR { t }
| NIL { nil() }
| u=term; PAR; v=term { par u v }
| FGT; x=kvl; t=term { fgt x t }
| LFT; t=term { lft t }
| p=PRM; t=term { prm p t }
| label=LABEL; x=kvl { edg (Info.kv "label" label :: x) }
(* syntactic sugar *)
| CNV; t=term { cnv t }
| i=INJ; t=term { inj i t }
| u=term; DOT; x=kvl; v=term { dot x u v }
| label=LABEL; HAT { dot [] (edg [Info.kv "label" label]) (edg [Info.kv "label" (label^"^")]) }
| STAR; x=kvl; LPAR; ts=separated_list(COMMA, term); RPAR { str x ts }
| SERIES; LPAR; ts=separated_list(COMMA, term); RPAR { ser ts }

kvl:
| LT; h=separated_list(SEMI, KEYVAL); k=kvl GT { h @ k }
| { [] }

sterm_:
| k=FIX; t=term { fixed (Seq.init k (fun _ -> [])) t }
| t=term { flexible (fun _ -> []) t }
| SHARP; h=separated_nonempty_list(COMMA, kvl); t=term { fixed (Seq.of_list h) t }

sterm:
| t=sterm_; EOF { t }

file:
| l=separated_nonempty_list(SIM, sterm_); EOF { l }

(* dot files *)
dotlines:
| b=BOX; SEMI; l=xdotlines { b,l }

xdotlines:
| i=ID; p=POS; SEMI; q=xdotlines { (i,p)::q }
| ID*; SEMI; q=xdotlines { q }
| EOF { [] }
  
