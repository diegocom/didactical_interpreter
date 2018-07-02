 (* File lexer.mll *)
        {
        open Parser        (* Tipo dei token definito nel file parser.mly *)
	open Syntax
        exception Eof
        }
        rule token = parse
          [' ' '\t' '\n']     { token lexbuf }     (* salto gli spazi bianchi *)
        | ";;"          	{ EOL }
	| "|"			{ EOL}
        | ['0'-'9']+ as lxm 	{ INT(int_of_string lxm) }
	| "T"			{TAINT(Syntax.T)}
	| "U"			{TAINT(Syntax.U)}
	| "false"	 	{ BOOL(false)}
	| "true"		{ BOOL(true)}
	| ['e' 'E']"int"        { EINT}
	| ['e' 'E']"string"	{ ESTRING }
	| ['e' 'E']"bool"	{ EBOOL }
	| "Sum" 		{ SUM } 
	| ['r' 'R']"eflect"	{tainted_r := true;REFLECT}
	| "Prod" 		{ PROD }
	| "Diff"		{ DIFF }
	| "Minus"		{ MINUS }
	| "Eq"			{ EQ }
	| ['o' 'O']"r"		{ OR }
	| ['a' 'A']"nd" 	{ AND }
	| ['i' 'I']"szero"	{ ISZERO }
     	| "Not"			{ NOT }
	| "Val"			{ VAL }
	| "Newloc"		{ NEWLOC }
	| "Ifthenelse"		{ IFTHENELSE }
	| "Fun"			{ FUN }
	| "in"			{ IN }
	| ['l' 'L']"et"		{ LET }
	| "Den"			{ DEN }
	| "Appl"		{ APPL }
	| "Block"		{ BLOCK }
	| "Call"		{ CALL }
	| "Proc"		{ PROC }
	| ['r' 'R']"ec"		{ REC }
        | '('            	{LPAREN }
        | ')'            	{ RPAREN }
	| ','			{ COMMA }
	| '['			{LQPAREN}
	| ']'			{RQPAREN}
	| ';'			{SEMIC}
	| "if"			{IF}
	| "then"		{THEN}
	| "else"		{ELSE}
	| "cif"			{CIF}
	| "Assign"		{ASSIGN}
	| "Cifthenelse" 	{CIFTHENELSE}
	| ['w' 'W']"hile" 	{WHILE}
	| "=="			{EQ}
	| "="			{EQUAL}
	| "+"			{SUM}
	| "-"			{MINUS}
	| "*"			{PROD}
	| "!"			{NOT}
	| "{"			{LBRACE}
	| "}"			{RBRACE}
	| "!="			{NOT_EQ}
	| "procedure"		{PROC}
	| "function"		{FUN}
	| "do"			{DO}
	| "runf"		{RUNF}
	| "runp"		{RUNP}
	| ['p' 'P']"rint"	{PRINT}
	| "len"			{LEN}
	| "substr"		{SUBSTR}
	| "concat"		{CONCAT}
	| "const"		{CONST}
	| '\\'	{ token lexbuf }
	| '.'		{CONCAT}
	| ("--SAFE MODE ON") {safe_mode:=true;token lexbuf}
	| ("--SAFE MODE OFF") {safe_mode:=false;token lexbuf}
	| ("--DEBUG MODE ON") {debug_mode:=true;token lexbuf}
	| ("--DEBUG MODE OFF") {debug_mode:=false;token lexbuf}
	| ("--PRINT ENV ON") {enable_print_env:=true;token lexbuf}
	| ("--PRINT STORE ON") {enable_print_store:=true;token lexbuf}
	| ("--PRINT ENV OFF") {enable_print_env:=false;token lexbuf}
	| ("--PRINT STORE OFF") {enable_print_store:=false;token lexbuf}
	| ("@@")['A'-'Z' 'a'-'z' '0'-'9' '!'-'/' ':' '<'-'@' '['-'?' '_' ' ' '|' ]* {token lexbuf}
	| ['A'-'Z' 'a'-'z']* as id {ID(id)}
	| '"'(['A'-'Z' 'a'-'z' '0'-'9' '!'-'/' ':' '<'-'@' '['-'?' '_' ' ' '|' '\\'] + as lxm)'"' {STRING(lxm)}
        | eof            { raise Eof }
