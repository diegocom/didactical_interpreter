
open Syntax;;
open Funenv;;
open Funstore;;
open Functions;;


(*Funzioni su procedure e funzioni*)
let rec makefun ((a:exp),(x:dval env)) =
      (match a with
      |	Fun(ii,aa) ->
      	  Dfunval(function (d,s) ->  sem aa (bindlist(x, ii, d)) s)
      |	_ -> failwith ("Non-functional object"))
      
and makeproc ((a:exp),(x:dval env)) =
    (match a with
      |	Proc(ii,b) ->
	  Dprocval(function (d1,s1) ->
			 semb b (bindlist(x, ii, d1)) s1) 
      |	_ -> failwith ("Non-procedural object"))
      
and applyfun ((ev1:dval),(ev2:dval list),s) =
      ( match ev1 with
      | Dfunval(x) -> x (ev2,s)
      | _ -> failwith ("attempt to apply a non-functional object"))
      
and applyproc ((ev1:dval),(ev2:dval list),s) =
      ( match ev1 with
      | Dprocval(x) -> x (ev2,s)
      | _ -> failwith ("attempt to apply a non-procedural object"))
      
and makefunrec (i, f, r) = match f with
	|Fun(ii, aa) ->
	  (let functional ff (d,s1) =
	    let r1 = bind(bindlist(r, ii, d), i, Dfunval(ff)) in
	    sem aa r1 s1 in
	  let rec fix =  function x -> functional fix x
	  in Funval(fix))
	| _ -> failwith("error makefunrec")
                
(* FUNZIONI DI VALUTAZIONE SEMANTICA *)

and sem (e:exp) (r:dval env) (s: mval store) =
  match e with
  (* TIPI BASE *)
  | Ebool(b,t) -> if(!tainted_r) then Bool(b,T) else Bool(b,t)
  | Eint(a,t) ->  if(!tainted_r) then Int(a,T) else Int(a,t)
  | Estring(s,t,c) ->  if(!tainted_r) then String(s,T,c) else String(s,t,c)
  (* BOOLEANI *)
  | And(a,b) -> et(sem a r s, sem b r s)
  | Or(a,b) -> vel(sem a r s, sem b r s)
  | Not(a) -> non(sem a r s)
  (* INTERI *)
  | Minus(a) -> minus(sem a r s)
  | Prod(a,b) -> mult(sem a r s, sem b r s)
  | Sum(a,b) -> plus(sem a r s, sem b r s)
  | Diff(a,b) -> diff(sem a r s, sem b r s)
  (* TEST SU INT *)
  | Eq(a,b) -> eq(sem a r s, sem b r s)
  | Iszero(a) -> zero(sem a r s)
  (* STRINGHE *)
  | Len(a) -> len(sem a r s)
  | Substr(a,x,y) -> sub(sem a r s, sem x r s, sem y r s)
  | Concat(a,b) -> conc(sem a r s, sem b r s)
  | Print(e) -> if(!debug_mode) then (print_string "\027[43mVALUTAZIONE:\027[0m  ");let value = (sem e r s) in print_val value;value
  (* ESPRESSIONI COMPOSTE *)
  | Ifthenelse(a,b,c) ->
     (match sem a r s with
     | Bool(x,t) ->
	if x then
	  (match sem b r s with
	  | Bool(y,z) -> Bool(y, op_and (t,z))
	  | Int(y,z) -> Int(y, op_and (t,z))
	  | String(y,z,c) -> String(y, op_and (t,z),c)
	  | _ -> failwith ("error type"))
	else
	  (match sem c r s with
	  | Bool(y,z) -> Bool(y, op_and (t,z))
	  | Int(y,z) -> Int(y, op_and (t,z))
	  | String(y,z,c) -> String(y, op_and (t,z),c)
	  | _ -> failwith ("error type"))
     | _ -> failwith ("nonboolean guard"))
  | Let(i,e1,e2) -> let (v, s1) = semden e1 r s in
		    sem e2 (bind (r,i,v)) s
  | Den(i) -> dvaltoeval(applyenv(r,i))
  | Val(e) -> let (v, s1) = semden e r s in
	      (match v with
	      | Dloc n -> mvaltoeval(applystore(s1, n))
	      | _ -> failwith ("not a variable"))
  | Fun(i,a) -> dvaltoeval(makefun(Fun(i,a), r))	  
  | Appl(a,b) -> let (v1,s1) = semlist b r s in
		 applyfun(evaltodval(sem a r s), v1, s)
  | Rec(f,e) -> makefunrec (f,e,r)

  | _ -> failwith("nonlegal expression for sem")

and semlist el r s = match el with
  | [] -> ([], s)
  | e::el1 -> let (v1,s1) = semden e r s in
	      let (v2,s2) = semlist el1 r s1 in
	      (v1::v2, s2)

and semden (e:exp) (r: dval env) (s: mval store) =
  match e with
  | Den(i) -> (applyenv(r,i), s)
  | Fun(i, e1) -> (makefun(e,r), s)
  | Proc(il,b) -> (makeproc(e,r), s)
  | Newloc(e) -> let m = evaltomval(sem e r s) in
		 let (l, s1) = allocate(s, m) in
		 (Dloc l, s1)
  | _ -> (evaltodval(sem e r s), s)

(* semantica delle dichiazioni di variabili *)
and semdv dl r s =
  match dl with
    | [] -> (r,s)
    | (i,e)::dl1 -> let (v, s1) = semden e r s in
		    semdv dl1 (bind(r, i, v)) s1

and semdl (dl,rl) r s =
  let (r1, s1) = semdv dl r s in
  semdr rl r1 s1

and semdr rl r s =
  let functional ((r1: dval env)) =
    (match rl with
    | [] -> r
    | (i,e) :: rl1 -> let (v,s2) = semden e r1 s in
		      let  (r2, s3) = semdr rl1 (bind(r, i, v)) s in
		      r2) in
  let rec rfix = function x -> functional rfix x in (rfix, s)
		
(* semantica dei comandi *)	
and semc (c: com) (r:dval env) (s: mval store) =
  match c with	
    | Assign(e1, e2) -> let (v1, s1) = semden e1 r s in
	(match v1 with
	   | Dloc(n) -> if(check_const(mvaltoeval(applystore(s1,n)))) then (failwith ("impossibile modificare costante")) else update(s1, n, evaltomval(sem e2 r s))
	   | _ -> failwith ("wrong location in assignment"))
    | Cifthenelse(e, cl1, cl2) ->
	(match sem e r s with
	| Bool(x,t) -> if x then semcl cl1 r s else semcl cl2 r s
	| _ -> failwith ("nonboolean guard"))
    | While(e, cl) ->
       	(* semantica di punto fisso del while *) 
     	let functional ((fi: mval store -> mval store)) = 
	  (function sigma ->
	     (match sem e r sigma with
		| Bool(x,t) -> if x then fi(semcl cl r sigma) else sigma
		| _ -> failwith ("nonboolean guard"))) in 
	let rec ssfix = (function x -> functional ssfix x) in
	  ssfix s
    | Block(b) -> semb b r s
    | Call(e1, e2) -> let (p,s1) = semden e1 r s in
		      let (v,s2) = semlist e2 r s1 in
		      applyproc(p, v, s2)
    | Reflect(e) -> failwith("not possible to use reflect here")
    
and semref e r s = 
	match e with
	| Reflect(e) -> (reflect (sem e r s) r s)
	| _ -> failwith("Error semref")
	
(* semantica di una lista di comandi *)	     
and semcl cl r s = match cl with
  | [] -> s
  | c::cl1 -> semcl cl1 r (semc c r s)

(* semantica di un blocco *)
and semb (dl, rdl, cl) r s = 
  let (r7, s7) = semdl (dl,rdl) r s in
  semcl cl r7 s7
(*semantica del parsing *)
and semt (l:parser) (r:dval env) (s: mval store) = match l with
	|Lexp(e) -> (sem e r s);(r, s)
	|Lcom(e) ->  let store = (semc e r s) in (r,store)
	|Ldecl(e) -> let (env,stor) = semdv e r s in (env,stor)
	|Lparse(e) -> let (env,stor) =(semref e r s) in (env,stor)
	| _ -> failwith("semt error")
(*applicazione della funzione di reflect su una lista di comandi/espressioni/dichiarazioni*)   
and reflect stringa r s =
  let elencoComandi = (Str.split (Str.regexp "|") (returnString stringa)) in
  parserL (elencoComandi) r s
(*parsing e applicazione di una lista di comandi/espressioni/dichiarazioni*)
and parserL x r s = match x with
  |[] -> tainted_r := false;(r,s)
  | h::t -> let (r10,s10) = (semt (parserR (h^"|")) r s) in parserL t r10 s10
(*parsing e ritorno di una lista di comandi/espressioni/dichiarazioni*)
and parserR x =  let lexbuf = (Lexing.from_string(x^"\n")) in 
	let comando = Parser.main Lexer.token lexbuf in comando;;
