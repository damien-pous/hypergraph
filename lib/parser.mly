%token LPAR RPAR LT GT COMMA SEMI STAR SERIES DOT QUOTE
%token PAR NIL LFT FGT EOF
%token <Common.label> LABEL
%token <Common.perm> PRM
%token <Common.inj> INJ
%token <int> FIX
%token <Info.kv> KEYVAL

%left PAR
%left DOT
%nonassoc LFT FGT PRM INJ FIX
%right QUOTE

%type <Common.info Raw.t> term
%type <Common.info Raw.t> main
%start main

%{
    open Raw    
%}

%%

term:
| LPAR; t=term; RPAR { t }
| NIL { nil' }
| u=term; PAR; v=term { par u v }
| FGT; x=kvli; t=term { fgt x t }
| LFT; t=term { lft t }
| p=PRM; t=term { prm p t }
| k=FIX; t=term { fix k t }
| label=LABEL; l=kvl { edg' (Info.of_kvl ~label l) }
(* syntactic sugar *)
| i=INJ; t=term { inj' i t }
| u=term; DOT; x=kvli; v=term { dot x u v }
| u=term; QUOTE { cnv u }
| STAR; x=kvli; LPAR; ts=separated_list(COMMA, term); RPAR { str x ts }
| SERIES; LPAR; ts=separated_list(COMMA, term); RPAR { ser ts }

kvl:
| LT; h=separated_list(SEMI, KEYVAL); GT; k=kvl { h @ k }
| { [] }

kvli:
| l = kvl { Info.of_kvl l }

main:
| t=term; EOF { t }
