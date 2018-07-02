open Syntax;;
open Funenv;;
open Funstore;;

(*-------------------------------FUNZIONI DI STAMPA---------------------------------------------------------*)

(*Funzione che permette di stampare il tipo taint*)
let print_taint value = 
	let print v = 
		match value with
	|U -> print_string "\027[32m Untaint\027[0m"
	|T -> print_string "\027[31m Taint\027[0m"
	in print value
;;

(*Funzione che permette di stampare il tipo eval*)
let print_val value = 
   let print v = 
	match value with
	|Int (a,b) -> (print_int a);print_string " ";print_taint b; (print_string "\n")
	|Bool (a,b) -> if a then (print_string "true ";print_taint b; (print_string "\n")) else (print_string "false ";print_taint b; (print_string "\n"))
	|String(a,b,c) -> (if(c) then print_string "const ");print_string a;print_string " ";print_taint b; (print_string "\n")
	|Funval(a) -> print_string("Funval\n")
	|Novalue -> print_string "No Value\n"
    in print value
;;

(*Funzione che permette di stampare una lista (lst), specificando una funzione di stampa per gli elementi (f)*)
let print_list f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; if(List.length t)!=0 then print_string ";"; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";;



(*Funzione di stampa delle espressioni*)
let rec print_expr exp =
    let rec print exp =   
      match exp with
       |Sum (a,b) -> print_string "Sum("; print a; print_string " , " ;
print b; print_string ")"
	|Eint (a,b) -> print_string "Eint("; (print_int a);print_string ",";print_taint b; (print_string ")")
	|Ebool (a,b) -> print_string "Ebool ";if a then (print_string "true";print_taint b; (print_string "\n")) else (print_string "false";print_taint b; (print_string "\n"))
	|Estring(a,b,c) -> print_string "Estring("; (print_string a);print_string ",";print_taint b;print_string ",";
if c then (print_string "true") else (print_string "false");
 (print_string ")")
	|Prod (a,b) -> print_string "Prod("; print a; print_string " , " ;
print b; print_string ")"
	|Diff (a,b) -> print_string "Diff("; print a; print_string " , " ;
print b; print_string ")"
        |Minus a -> print_string "Minus "; print a
	|Eq (a,b) -> print_string "Eq("; print a; print_string " , " ;
print b; print_string ")"
	|Or (a,b) -> print_string "Or("; print a; print_string " , " ;
print b; print_string ")"
	|And (a,b) -> print_string "And("; print a; print_string " , " ;
print b; print_string ")"
	|Iszero a -> print_string "Iszero "; print a
	|Not a -> print_string "Not "; print a
	(*|Den a -> print_string "Den "; print 0 a*)
	|Val a -> print_string "Val "; print a
	|Newloc a -> print_string "NewLoc "; print a	
	|Ifthenelse (a,b,c) -> print_string "Ifthenelse("; print a; print_string " , " ;
print b; print_string " , " ;print c; print_string ")"
	|Let (a,b,c) -> print_string "Let("; print_string a; print_string " , " ;
print b; print_string " , " ;print c; print_string ")"
	|Den (a) -> print_string "Den("; print_string a; print_string ")"
	|Fun (a,b) -> print_string "Fun("; print_list print_string a; print_string ","; print b; print_string ")"
	|Appl(a,b) -> print_string "Appl("; print a; print_string ","; print_list print_expr b; print_string ")"
	|Rec(a,b) -> print_string "Rec("; print_string a; print_string "," ; print b; print_string ")"
	|Proc(a,b) -> print_string "Proc(";print_list print_string a; print_string "," ; print_block b; print_string ")"
	|Len (a) -> print_string "Len("; print a; print_string ")"
	|Print (a) -> print_string "Print("; print a; print_string ")"
	|Substr (a,b,c) -> print_string "Substr("; print a; print_string " , " ;
print b; print_string " , " ;print c; print_string ")"
	|Concat (a,b) -> print_string "Concat("; print a; print_string " , " ;
print b; print_string ")"
    in print exp

(*funzione di stampa delle dichiarazioni*)
and print_decl e = 
	match e with
	|(a,b) -> print_string "(";print_string a;print_string ",";print_expr b;print_string ")"
(*funzione di stampa dei blocchi*)
and print_block b = 
 	match b with
	|(a,b,c) -> print_string "(";print_list print_decl a;print_string ",";print_list print_decl b;print_string ",";print_list print_comm c;print_string ")"
(*funzione di stampa dei comandi*)	
and print_comm c =
	match c with
          | Assign(e1,e2) -> print_string "Assign(";print_expr e1;print_string ",";print_expr e2;print_string ")"                      (* assegnamento *)
	  | Cifthenelse(e,cl1,cl2) -> print_string "Cifthenelse("; print_expr e; print_string " , " ;
print_list print_comm cl1; print_string " , " ;print_list print_comm cl2; print_string ")"
	  | While(e,cl1) -> print_string "While("; print_expr e; print_string " , " ;
print_list print_comm cl1; print_string ")" 
	  | Block(b) -> print_block b
	  | Call(e,el) -> print_string "Call("; print_expr e; print_string " , " ;print_list print_expr el; print_string ")"
		 (* REFLECT *)
	  | Reflect(e) -> print_string "Reflect(";print_expr e;print_string ")"
and print_ide_exp v = 
	match v with
	|(i,e) -> print_string "(";print_string i;print_string ",";print_expr e;print_string ")"

(*Funzione di stampa del tipo parser*)
and print_parser p = 
let print a = 
	match a with
	|Lexp(e) -> print_expr e
	|Ldecl(e) -> print_list print_decl e
	|Lcom(c) -> print_comm c
	|Lparse(e) -> print_comm e
	|_ -> print_string("Non implementato")
in print_string "\027[36m IN ESECUZIONE ISTRUZIONE: \027[0m\n";print p;print_string "\n\n";;

(*Funzione che stampa una lista di stringhe*)
let rec stampa x = match x with
  |[] -> print_string ""
  | h::t -> print_string (h ^ "\n"); stampa t;;


(*Funzione che mi permette di stampare l'intero store*)
let rec print_store s i = 
	let value = mvaltoeval(applystore(s,i)) in 
		match value with
		|Novalue -> print_string "\n"
		| _ -> print_string "Locazione ";print_int i; print_string ": ";print_val value;print_store s (i+1);;

(*Funzione che mi permette di stampare una locazione dello store*)
let rec print_loc s i = 
	let value = mvaltoeval(applystore(s,i)) in 
		match value with
		|Novalue -> print_string "Novalue\n"
		| _ -> print_val value;;

(*Funzione che mi permette di stampare l'ambiente prendendo in input la lista degli identificatori usati*)
let rec print_env lista r s = match lista with
  |[] -> print_string ""
  | h::t -> print_string "Identificatore ";print_string h; let val_env = applyenv(r,h) in
							(match val_env with
						 	  | Dint(u,t) -> print_string " -> "; print_val (dvaltoeval(val_env))
							  | Dbool(u,t)-> print_string " -> "; print_val (dvaltoeval(val_env))
							  | Dstring(u,t,c)-> print_string " -> "; print_val (dvaltoeval(val_env))
							  | Dloc(u) -> print_string " -> locazione ";print_int u;print_string " -> "; print_loc s u
							  | Dfunval(u) -> print_string " -> Dfunval\n"
							  | Dprocval(u) -> print_string " -> Dprocval\n"
							  | Unbound -> print_string " -> Unbound\n")
							;print_env t r s;;


(*-------------------------------------------FINE FUNZIONI DI STAMPA  ------------------------------------------------*)

(*-------------------------------------------FUNZIONI SULLE STRINGHE ------------------------------------------------*)

(*FUNZIONE CHE UNISCE UNA LISTA DI STRINGHE*)
let rec unisci x = match x with
   | [] -> ""
  | h::t -> h^(unisci t);;

(*Funzione che ritorna una stringa da un tipo eval String*)
let returnString e = match e with
	| String(t,u,c) -> t
	| _ -> failwith("error return string");;

(*Funzione che ritorna un boolean indicante se una stringa è costante o meno*)
let check_const(e) = 
	match e with
	| String(t,u,c) -> c
	| _ -> false
(*-------------------------------------------FINE FUNZIONI SULLE STRINGHE ------------------------------------------------*)

