 /* File parser.mly */
%{
	open Syntax;;
%}
	%token <string> STRING
        %token <int> INT 
	%token <bool> BOOL
	%token <Syntax.taint> TAINT
	%token <string> ID
	%token CONST
	%token EINT ESTRING EBOOL SUM PROD DIFF MINUS OR AND ISZERO NOT NEWLOC IFTHENELSE LET DEN FUN APPL REC NOT_EQ PRINT RUNF EQUAL IF THEN ELSE PROC SUBSTR CONCAT LEN IN VAL EQ
        %token CALL BLOCK RUNP CIF ASSIGN CIFTHENELSE WHILE DO
	%token REFLECT 
        %token LPAREN RPAREN COMMA LQPAREN RQPAREN SEMIC RBRACE LBRACE
        %token EOL
        %start main             /* the entry point */
        %type <Syntax.parser> main
	%type <Syntax.ide> ideval
	%type <Syntax.ide list> idevall
	%type <Syntax.exp list> exprl
	%type <Syntax.exp > expr
        %type <Syntax.exp list> expr_f_l
	%type <Syntax.exp > expr_f
	%type <Syntax.exp > expr_p
	%type <Syntax.com> comm
	%type <Syntax.com list> comml
	%type <Syntax.dic > dicl
	%type <Syntax.dic> dic
	%type <Syntax.block> block
	%type <Syntax.com> reflect
	%type <Syntax.exp> base_type
	%left CONST
	%left EQ
	%left VAL
        %%


	/*ENTRY POINT*/
        main:
           | expr EOL                { Lexp $1 }
	   | comm EOL		     { Lcom $1 }
	   | dic EOL		     { Ldecl $1 }
	   | reflect EOL	     { Lparse $1 }
	   | EOL 		     { Lexp(Eint(0,U))}
        ;


	/*TIPI BASE*/
	base_type:
	 | LPAREN INT COMMA TAINT RPAREN         { Eint ($2,$4) }
	 | LPAREN BOOL COMMA TAINT RPAREN	{ Ebool ($2,$4) }
	 | LPAREN STRING COMMA TAINT RPAREN	{ Estring ($2,$4,false) }
	 | LPAREN base_type RPAREN { ($2) }
	 | INT          { Eint ($1,U) }
	 | BOOL 	{ Ebool ($1,U) }
	 | STRING 	{ Estring ($1,U,false) }
	;

	/*TIPI BASE COSTANTI*/
	base_type_c:
	 | LPAREN STRING COMMA TAINT RPAREN	{ Estring ($2,$4,true) }
	 | STRING 	{ Estring ($1,U,true) }
	 | LPAREN base_type_c RPAREN { $2 }
	;
	
	
	base_type_l:
	| base_type { $1::[]}
	| base_type SEMIC base_type_l { $1::$3 }
	| LQPAREN base_type_l RQPAREN { $2 } 
 
	;

	
	/*REFLECT*/

	reflect:
 	| REFLECT LPAREN expr RPAREN { Reflect($3) }
	| REFLECT expr { Reflect($2) }
	;
	
	/*COMANDI*/

	comm:
	| ASSIGN LPAREN expr COMMA expr RPAREN	{ Assign($3, $5) }
	| ideval EQUAL expr	{ Assign(Den $1,$3) }
	| CIFTHENELSE LPAREN expr COMMA comml COMMA comml RPAREN { Cifthenelse($3,$5,$7) }
	| WHILE LPAREN expr_p COMMA comml RPAREN { While($3,$5) }
	| WHILE expr_p COMMA comml { While($2,$4) }
	| WHILE expr_p DO comm_p_l { While($2,$4) }
	| CIF expr_p THEN comm_p_l ELSE comm_p_l { Cifthenelse($2,$4,$6) }
	| BLOCK block {Block($2)}
	| LBRACE block RBRACE {Block($2)}
	| CALL LPAREN expr COMMA exprl RPAREN { Call($3,$5) }
	| RUNP ideval LPAREN expr_p_l RPAREN { Call(Den $2,$4) }
	;

	comml:
	| LQPAREN comml RQPAREN { $2 }
	| LPAREN comml RPAREN { $2 }
	| comm SEMIC comml { $1::$3 }
	| comm  { $1::[] }
	| LQPAREN RQPAREN	{[]}
	;


	/*COMANDI*/

	comm_p:
	| ASSIGN LPAREN expr COMMA expr RPAREN	{ Assign($3, $5) }
	| ideval EQUAL expr_p	{ Assign(Den $1,$3) }
	| CIFTHENELSE LPAREN expr COMMA comml COMMA comml RPAREN { Cifthenelse($3,$5,$7) }
	| WHILE LPAREN expr_p COMMA comml RPAREN { While($3,$5) }
	| WHILE expr_p COMMA comml { While($2,$4) }
	| WHILE expr_p DO comml { While($2,$4) }
	| CIF expr_p THEN comml ELSE comml { Cifthenelse($2,$4,$6) }
	| BLOCK block {Block($2)}
	| LBRACE block RBRACE {Block($2)}
	| CALL LPAREN expr COMMA exprl RPAREN { Call($3,$5) }
	| RUNP ideval LPAREN expr_p_l RPAREN { Call(Den $2,$4) }
	;

	comm_p_l:
	| LQPAREN comm_p_l RQPAREN { $2 }
	| LPAREN comm_p_l RPAREN { $2 }
	| comm_p SEMIC comm_p_l { $1::$3 }
	| comm_p { $1::[] }
	| LQPAREN RQPAREN	{[]}
	;

	/*DICHIARAZIONI*/

	dicl:
	| dics SEMIC dicl { $1::$3 }
	| dics  { $1::[] }
	;

	dic:
	| dicl	{$1}
	| LPAREN dic RPAREN {$2}
	| LQPAREN dicl RQPAREN { $2 }	
	;

	dics:
	| LPAREN ideval COMMA expr RPAREN  { ($2,$4 ) }
	| EINT ideval EQUAL base_type {($2, Newloc($4))}
	| EINT ideval EQUAL expr_f {($2, Newloc($4))}
	| ESTRING ideval EQUAL base_type {($2, Newloc($4))}
	| CONST ESTRING ideval EQUAL base_type_c {($3, Newloc($5))}
	| ESTRING ideval EQUAL expr_f {($2, Newloc($4))}
	| EBOOL ideval EQUAL base_type {($2, Newloc($4))}
	| EBOOL ideval EQUAL expr_f {($2, Newloc($4))}
	| PROC ideval LPAREN idevall RPAREN EQUAL block {($2,Proc($4,$7))}
	| FUN ideval LPAREN idevall RPAREN EQUAL expr_f { ($2,Fun($4,$7))}
	| REC FUN ideval LPAREN idevall RPAREN EQUAL expr_f { ($3,Rec($3,Fun($5,$8)))}
	;
	

	
	/*ESPRESSIONI*/

        expr:
        | base_type { $1 }
        | LPAREN expr RPAREN      { $2 }

	/*TEST SU INT*/

	| EQ LPAREN expr COMMA expr RPAREN {Eq($3, $5)}
	| ISZERO expr {Iszero $2}
	| expr EQ expr {Eq($1,$3)}
	| expr NOT_EQ expr	{Not(Eq($1,$3))}

	/*COSTANTI*/

	| EINT base_type { $2 }
	| EBOOL base_type { $2 }
	| ESTRING base_type { $2 }
	| CONST ESTRING base_type_c { $3 }
   	
	/*IDENTIFICATORI*/

	| VAL expr {(Val($2))}
	| DEN ideval	{ Den($2)}

	/*OPERAZIONI SU BOOL*/

	| OR LPAREN expr COMMA expr RPAREN {Or($3, $5)}
	| AND LPAREN expr COMMA expr RPAREN {And($3, $5)}
	| NOT expr 	{Not $2}
	| expr OR expr 	{Or($1,$3)}
	| expr AND expr {And($1,$3)}

	
	/*OPERAZIONI SU INT*/

	| PROD LPAREN expr COMMA expr RPAREN {Prod($3, $5)}
	| DIFF LPAREN expr COMMA expr RPAREN {Diff($3, $5)}
	| MINUS expr {Minus $2}
	| SUM LPAREN expr COMMA expr RPAREN           { Sum($3, $5) }
	| MINUS expr	{Minus $2}
	| expr SUM expr 	{Sum($1,$3)}
	| expr MINUS expr 	{Diff($1,$3)}
	| expr PROD expr 	{Prod($1,$3)}
	

	/*ESPRESSIONI COMPOSTE*/

	| IFTHENELSE LPAREN expr COMMA expr COMMA expr RPAREN {Ifthenelse($3, $5, $7)}
	| IF LPAREN expr RPAREN THEN expr ELSE expr { Ifthenelse($3,$6,$8) }
	| IF LPAREN expr RPAREN THEN expr { Ifthenelse($3,$6,Eint (0,T)) }
	| LET LPAREN ideval COMMA expr COMMA expr RPAREN	{ Let($3,$5,$7) }
	| LBRACE ideval EQUAL expr COMMA expr RBRACE {Let($2,$4,$6)}
	| LET FUN ideval LPAREN idevall RPAREN EQUAL expr_f IN expr_f {Let($3,Fun($5,$8),$10)}
	| LET REC FUN ideval LPAREN idevall RPAREN EQUAL expr_f IN expr_f {Let($4,Rec($4,Fun($6,$9)),$11)}
	| LET ideval EQUAL expr_f IN expr_f { Let($2,$4,$6) }

	/*LOCAZIONE*/
	| NEWLOC expr {Newloc $2}

	/*FUNZIONI*/
	| FUN LPAREN idevall COMMA expr RPAREN	{ Fun($3, $5) }
	/*| FUN idevall LPAREN expr RPAREN {Fun($2,$4)}*/
	| APPL LPAREN expr COMMA expr_f_l RPAREN	{ Appl($3, $5) }
	| REC LPAREN ideval COMMA expr RPAREN	{ Rec($3, $5) }
	| PROC LPAREN idevall COMMA block RPAREN {Proc($3,$5)}
	| PROC idevall LBRACE block RBRACE {Proc($2,$4)}
	| RUNF ideval LPAREN expr_p_l RPAREN { Appl(Den $2,$4) }
	
	
	/*OPERAZIONI SU STRINGHE*/
        | PRINT expr_p {Print($2)}
        | LEN expr {Len($2)}
	| SUBSTR LPAREN expr COMMA expr COMMA expr RPAREN {Substr($3,$5,$7)}
	| CONCAT LPAREN expr COMMA expr RPAREN {Concat($3,$5)}
	| expr CONCAT expr {Concat($1,$3)}
	;
        /*VALORI*/
	val_den:
	| expr_f { Val($1) }
	;

	
	/*IDENTIFICATORI*/

	ideval:
	| ID { $1 }
	| LPAREN ideval RPAREN { $2 }
	;
	
	
	idevall:
	| ideval { $1::[]}
	| ideval SEMIC idevall { $1::$3 }
	| LQPAREN idevall RQPAREN { $2 } 
 
	;
	exprl:
	| LQPAREN exprl RQPAREN { $2 }
	| val_den SEMIC exprl { $1::$3 }
	| val_den  { $1::[] }
	;
	
	/*BLOCCHI*/
	block:
	| LPAREN block RPAREN	{ ($2) }
	| LQPAREN RQPAREN COMMA LQPAREN RQPAREN COMMA LQPAREN RQPAREN  { ([],[],[]) }
	| dic COMMA LQPAREN RQPAREN COMMA LQPAREN RQPAREN  { ($1,[],[]) }
	| dic COMMA dic COMMA LQPAREN RQPAREN  { ($1,$3,[]) }
	| LQPAREN RQPAREN COMMA dic COMMA LQPAREN RQPAREN  { ([],$4,[]) }
	| dic COMMA LQPAREN RQPAREN COMMA comml  { ($1,[],$6) }
	| LQPAREN RQPAREN COMMA LQPAREN RQPAREN COMMA comml  { ([],[],$7) }
	| LQPAREN RQPAREN COMMA dic COMMA comml RPAREN { ([],$4,$6) }
	| LPAREN dic COMMA dic COMMA comml RPAREN { ($2,$4,$6) }
	| dic COMMA dic COMMA comml { ($1,$3,$5) }
	| comml {([],[],$1)}
	| dic COMMA comml { ($1,[],$3) }
	| dic	{ ($1,[],[]) }
	| LBRACE block RBRACE { $2 }
	;



	/*ESPRESSIONI*/

        expr_f:
	| ID { Den($1) }
        | base_type { $1 }
        | LPAREN expr_f RPAREN      { $2 }

	/*TEST SU INT*/

	| EQ LPAREN expr_f COMMA expr_f RPAREN {Eq($3, $5)}
	| ISZERO expr_f {Iszero $2}
	| expr_f EQ expr_f {Eq($1,$3)}
	| expr_f NOT_EQ expr_f	{Not(Eq($1,$3))}

	/*COSTANTI*/

	| EINT base_type { $2 }
	| EBOOL base_type { $2 }
	| ESTRING base_type { $2 }
	| CONST ESTRING base_type_c { $3 }
   	
	/*IDENTIFICATORI*/

	| VAL expr_f {(Val($2))}
	| DEN ideval	{ Den($2)}

	/*OPERAZIONI SU BOOL*/

	| OR LPAREN expr_f COMMA expr_f RPAREN {Or($3, $5)}
	| AND LPAREN expr_f COMMA expr_f RPAREN {And($3, $5)}
	| NOT expr_f 	{Not $2}
	| expr_f OR expr_f 	{Or($1,$3)}
	| expr_f AND expr_f {And($1,$3)}

	
	/*OPERAZIONI SU INT*/

	| PROD LPAREN expr_f COMMA expr_f RPAREN {Prod($3, $5)}
	| DIFF LPAREN expr_f COMMA expr_f RPAREN {Diff($3, $5)}
	| MINUS expr_f {Minus $2}
	| SUM LPAREN expr_f COMMA expr_f RPAREN           { Sum($3, $5) }
	| MINUS expr_f	{Minus $2}
	| expr_f SUM expr_f 	{Sum($1,$3)}
	| expr_f MINUS expr_f 	{Diff($1,$3)}
	| expr_f PROD expr_f 	{Prod($1,$3)}
	

	/*ESPRESSIONI COMPOSTE*/

	| IFTHENELSE LPAREN expr_f COMMA expr_f COMMA expr_f RPAREN {Ifthenelse($3, $5, $7)}
	| IF LPAREN expr_f RPAREN THEN expr_f ELSE expr_f { Ifthenelse($3,$6,$8) }
	| IF LPAREN expr_f RPAREN THEN expr_f { Ifthenelse($3,$6,Eint (0,T)) }
	| LET LPAREN ideval COMMA expr_f COMMA expr_f RPAREN	{ Let($3,$5,$7) }
	| LBRACE ideval EQUAL expr_f COMMA expr_f RBRACE {Let($2,$4,$6)}
	| LET FUN ideval LPAREN idevall RPAREN EQUAL expr_f IN expr_f {Let($3,Fun($5,$8),$10)}
	| LET REC FUN ideval LPAREN idevall RPAREN EQUAL expr_f IN expr_f {Let($4,Rec($4,Fun($6,$9)),$11)}
	| LET ideval EQUAL expr_f IN expr_f { Let($2,$4,$6) }

	/*LOCAZIONE*/
	| NEWLOC expr_f {Newloc $2}

	/*FUNZIONI*/
	| FUN LPAREN idevall COMMA expr_f RPAREN	{ Fun($3, $5) }
	/*| FUN idevall LPAREN expr_f RPAREN {Fun($2,$4)}*/
	| APPL LPAREN expr_f COMMA expr_f_l RPAREN	{ Appl($3, $5) }
	| REC LPAREN ideval COMMA expr_f RPAREN	{ Rec($3, $5) }
	| PROC LPAREN idevall COMMA block RPAREN {Proc($3,$5)}
	| PROC idevall LBRACE block RBRACE {Proc($2,$4)}
	| RUNF ideval LPAREN expr_f_l RPAREN { Appl(Den $2,$4) }
	
	/*OPERAZIONI SU STRINGHE*/
        | PRINT expr_f {Print($2)}
	
        | LEN expr_f {Len($2)}
	| SUBSTR LPAREN expr_f COMMA expr_f COMMA expr_f RPAREN {Substr($3,$5,$7)}
	| CONCAT LPAREN expr_f COMMA expr_f RPAREN {Concat($3,$5)}
	| expr_f CONCAT expr_f {Concat($1,$3)}
	;


	expr_f_l:
	| LQPAREN expr_f_l RQPAREN { $2 }
	| expr_f SEMIC expr_f_l { $1::$3 }
	| expr_f  { $1::[] }
	;




	expr_p_l:
	| LQPAREN expr_p_l RQPAREN { $2 }
	| expr_p SEMIC expr_p_l { $1::$3 }
	| expr_p  { $1::[] }
	;




	/*ESPRESSIONI PROCEDURE*/

        expr_p:
	| ID { Val(Den($1)) }
        | base_type { $1 }
        | LPAREN expr_p RPAREN      { $2 }

	/*TEST SU INT*/

	| EQ LPAREN expr_p COMMA expr_p RPAREN {Eq($3, $5)}
	| ISZERO expr_p {Iszero $2}
	| expr_p EQ expr_p {Eq($1,$3)}
	| expr_p NOT_EQ expr_p	{Not(Eq($1,$3))}

	/*COSTANTI*/

	| EINT base_type { $2 }
	| EBOOL base_type { $2 }
	| ESTRING base_type { $2 }
	| CONST ESTRING base_type_c { $3 }
   	
	/*IDENTIFICATORI*/

	| VAL expr_p {(Val($2))}
	| DEN ideval	{ Den($2)}

	/*OPERAZIONI SU BOOL*/

	| OR LPAREN expr_p COMMA expr_p RPAREN {Or($3, $5)}
	| AND LPAREN expr_p COMMA expr_p RPAREN {And($3, $5)}
	| NOT expr_p 	{Not $2}
	| expr_p OR expr_p 	{Or($1,$3)}
	| expr_p AND expr_p {And($1,$3)}

	
	/*OPERAZIONI SU INT*/

	| PROD LPAREN expr_p COMMA expr_p RPAREN {Prod($3, $5)}
	| DIFF LPAREN expr_p COMMA expr_p RPAREN {Diff($3, $5)}
	| MINUS expr_p {Minus $2}
	| SUM LPAREN expr_p COMMA expr_p RPAREN           { Sum($3, $5) }
	| MINUS expr_p	{Minus $2}
	| expr_p SUM expr_p 	{Sum($1,$3)}
	| expr_p MINUS expr_p 	{Diff($1,$3)}
	| expr_p PROD expr_p 	{Prod($1,$3)}
	

	/*ESPRESSIONI COMPOSTE*/

	| IFTHENELSE LPAREN expr_p COMMA expr_p COMMA expr_p RPAREN {Ifthenelse($3, $5, $7)}
	| IF LPAREN expr_p RPAREN THEN expr_p ELSE expr_p { Ifthenelse($3,$6,$8) }
	| IF LPAREN expr_p RPAREN THEN expr_p { Ifthenelse($3,$6,Eint (0,T)) }
	| LET LPAREN ideval COMMA expr_p COMMA expr_p RPAREN	{ Let($3,$5,$7) }
	| LBRACE ideval EQUAL expr_p COMMA expr_p RBRACE {Let($2,$4,$6)}
	| LET FUN ideval LPAREN idevall RPAREN EQUAL expr_f IN expr_f {Let($3,Fun($5,$8),$10)}
	| LET REC FUN ideval LPAREN idevall RPAREN EQUAL expr_f IN expr_f {Let($4,Rec($4,Fun($6,$9)),$11)}
	

	/*LOCAZIONE*/
	| NEWLOC expr_p {Newloc $2}

	/*FUNZIONI*/
	| FUN LPAREN idevall COMMA expr_p RPAREN	{ Fun($3, $5) }
	/*| FUN idevall LPAREN expr_p RPAREN {Fun($2,$4)}*/
	| APPL LPAREN expr_p COMMA expr_f_l RPAREN	{ Appl($3, $5) }
	| REC LPAREN ideval COMMA expr_p RPAREN	{ Rec($3, $5) }
	| PROC LPAREN idevall COMMA block RPAREN {Proc($3,$5)}
	| PROC idevall LBRACE block RBRACE {Proc($2,$4)}
	| RUNF ideval LPAREN expr_f_l RPAREN { Appl(Den $2,$4) }
	
	
	/*OPERAZIONI SU STRINGHE*/
        | PRINT val_den {Print($2)}
        | LEN expr_p {Len($2)}
	| SUBSTR LPAREN expr_p COMMA expr_p COMMA expr_p RPAREN {Substr($3,$5,$7)}
	| CONCAT LPAREN expr_p COMMA expr_p RPAREN {Concat($3,$5)}
	| expr_p CONCAT expr_p {Concat($1,$3)}
	;






