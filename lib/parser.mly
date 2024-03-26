%token LPAR RPAR LT GT COMMA SEMI STAR SERIES DOT QUOTE SHARP
%token PAR NIL LFT FGT SIM EOF
%token <Types.label> LABEL
%token <Types.perm> PRM
%token <Types.inj> INJ
%token <int> FIX
%token <Info.kv> KEYVAL

%left PAR
%left DOT
%nonassoc LFT FGT PRM INJ
%right QUOTE

%type <Info.kvl Raw.t> sterm
%start sterm

%type <Info.kvl Raw.t list> file
%start file



(* for parsing dot files and extracting positions *)
%token <Gg.p2> POS
%token <Gg.box2> BOX
%token <Types.kind*int> ID

%type <Gg.box2*(((Types.kind*int)*Gg.p2) list)> dotlines
%start dotlines

%{
    open Raw
%}

%%

term:
| LPAR; t=term; RPAR { t }
| NIL { Nil }
| u=term; PAR; v=term { Par(u,v) }
| FGT; x=kvl; t=term { Fgt(x,t) }
| LFT; t=term { Lft t }
| p=PRM; t=term { Prm(p,t) }
| label=LABEL; x=kvl { Edg (Info.kv "label" label :: x) }
(* syntactic sugar *)
| i=INJ; t=term { Inj(i,t) }
| u=term; DOT; x=kvl; v=term { Dot(x,u,v) }
| u=term; QUOTE { Cnv u }
| STAR; x=kvl; LPAR; ts=separated_list(COMMA, term); RPAR { Str(x,ts) }
| SERIES; LPAR; ts=separated_list(COMMA, term); RPAR { Ser ts }

kvl:
| LT; h=separated_list(SEMI, KEYVAL); k=kvl GT { h @ k }
| { [] }

sterm_:
| k=FIX; t=term { source (Seq.init k (fun _ -> [])) t }
| t=term { flexible (fun _ -> []) t }
| SHARP; h=separated_nonempty_list(COMMA, kvl); t=term {source (Seq.of_list h) t}

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
  
