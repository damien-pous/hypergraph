%token LPAR RPAR LT GT COMMA SEMI STAR SERIES DOT QUOTE SHARP
%token PAR NIL LFT FGT EOF
%token <Types.label> LABEL
%token <Types.perm> PRM
%token <Types.inj> INJ
%token <int> FIX
%token <Info.kv> KEYVAL

%left PAR
%left DOT
%nonassoc LFT FGT PRM INJ
%right QUOTE

%type <Info.kvl Raw.t> main
%start main

%{
    open Raw
    open U
%}

%%

term:
| LPAR; t=term; RPAR { t }
| NIL { nil' }
| u=term; PAR; v=term { par u v }
| FGT; x=kvl; t=term { fgt x t }
| LFT; t=term { lft t }
| p=PRM; t=term { prm p t }
| label=LABEL; x=kvl { edg' (Info.kv "label" label :: x) }
(* syntactic sugar *)
| i=INJ; t=term { inj' i t }
| u=term; DOT; x=kvl; v=term { dot x u v }
| u=term; QUOTE { cnv u }
| STAR; x=kvl; LPAR; ts=separated_list(COMMA, term); RPAR { str x ts }
| SERIES; LPAR; ts=separated_list(COMMA, term); RPAR { ser ts }

kvl:
| LT; h=separated_list(SEMI, KEYVAL); k=kvl GT { h @ k }
| { [] }

sterm:
| k=FIX; t=term { source (Seq.init k (fun _ -> [])) t }
| t=term { flexible (fun _ -> []) t }
| SHARP; h=separated_nonempty_list(COMMA, kvl); t=term {source (Seq.of_list h) t}

main:
| t=sterm; EOF { t }
