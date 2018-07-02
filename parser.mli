type token =
  | STRING of (string)
  | INT of (int)
  | BOOL of (bool)
  | TAINT of (Syntax.taint)
  | ID of (string)
  | CONST
  | EINT
  | ESTRING
  | EBOOL
  | SUM
  | PROD
  | DIFF
  | MINUS
  | OR
  | AND
  | ISZERO
  | NOT
  | NEWLOC
  | IFTHENELSE
  | LET
  | DEN
  | FUN
  | APPL
  | REC
  | NOT_EQ
  | PRINT
  | RUNF
  | EQUAL
  | IF
  | THEN
  | ELSE
  | PROC
  | SUBSTR
  | CONCAT
  | LEN
  | IN
  | VAL
  | EQ
  | CALL
  | BLOCK
  | RUNP
  | CIF
  | ASSIGN
  | CIFTHENELSE
  | WHILE
  | DO
  | REFLECT
  | LPAREN
  | RPAREN
  | COMMA
  | LQPAREN
  | RQPAREN
  | SEMIC
  | RBRACE
  | LBRACE
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.parser
