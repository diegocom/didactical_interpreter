(*Creazione di una lista per tenere traccia dell'elenco degli identificatori e stampare la memoria*)
let list_var = ref ([]);;

(*implementazione dell'ambiente*)
module Funenv =
struct
  type 't env = string -> 't
  exception WrongBindList
  let emptyenv(x) = function y -> x
  let applyenv(x,y) = x y
  let bind((r: 'a env), (l:string), (e:'a)) =
	list_var := (l::!list_var);
    function lu -> if lu = l then e else applyenv(r, lu)
  let rec bindlist(r, il, el) = match (il,el) with
    | ([],[]) -> r
    | i::il1, e::el1 -> bindlist (bind(r, i, e), il1, el1)
    | _ -> raise WrongBindList

end
open Funenv;;

(* implementazione funzionale store *)
module Funstore =

struct
  type loc = int
  type 't store = loc -> 't 

  let (newloc,initloc) =
    let count = ref(-1) in
      ((fun () -> count := !count +1; !count),
       (fun () -> count := -1))
      
  (* store vuoto *)
  let emptystore(x) = initloc(); function y -> x

  (* trova valore loc in store *)
  let applystore(x,y) = x y

  (* alloca loc in store *)
  let allocate((r: 'a store), (e:'a)) =
    let l = newloc() in
      (l, function lu -> if lu = l then e else applystore(r,lu))
 
  (* aggiorna loc in store *)
  let update((r: 'a store), (l:loc), (e:'a)) = 
    function lu -> if lu = l then e else applystore(r,lu)    
end

open Funstore;;

(*definizione dei boolean per le direttive dell'interprete*)
let tainted_r = ref false;;
let safe_mode = ref false;;
let debug_mode = ref false;;
let enable_print_store = ref false;;
let enable_print_env = ref false;;

(*dichiarazione delle eccezioni*)
exception Nonstorable
exception Nonexpressible


type ide = string

and taint =
  | T
  | U
      
and exp =
  
  (* COSTANTI *)
  | Ebool of bool * taint
  | Eint of int * taint
  | Estring of string * taint * bool
      
  (* OPERAZIONI SU BOOL *)
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
      
  (* OPERAZIONI SU INT *)
  | Minus of exp
  | Prod of exp * exp
  | Sum of exp * exp
  | Diff of exp * exp

  (* TEST SU INT *)
  | Eq of exp * exp
  | Iszero of exp

  (* ESPRESSIONI COMPOSTE *)
  | Ifthenelse of exp * exp * exp
  | Let of ide * exp * exp

  (* IDENTIFICATORI *)
  | Den of ide
  | Val of exp

  (* LOCAZIONE *)
  | Newloc of exp

  (* FUNZIONI *)
  | Fun of ide list * exp
  | Appl of exp * exp list
  | Rec of ide * exp
  | Proc of ide list * block
      
  (* OPERAZIONI SU STRINGHE *)
  | Len of exp
  | Substr of exp * exp * exp
  | Concat of exp * exp
  | Print of exp
 

(* dichiarazioni *)
and decl = (ide * exp) list * (ide * exp) list
and dic = (ide * exp) list 

(* blocco *)
and block = (ide * exp) list * (ide * exp) list * com list
	
(* comandi *)
and com = 
  | Assign of exp * exp                       (* assegnamento *)
  | Cifthenelse of exp * com list * com list  (* if-then-else *)
  | While of exp * com list                   (* while *)
  | Block of block                            (* blocco *)
  | Call of exp * exp list
	 (* REFLECT *)
  | Reflect of exp
    
and eval =
  | Int of int * taint
  | Bool of bool * taint
  | String of string * taint * bool
  | Funval of efun
  | Novalue
and efun = (dval list) * (mval store) -> eval
and proc = (dval list) * (mval store) -> mval store
and dval =
  | Dint of int * taint
  | Dbool of bool * taint
  | Dstring of string * taint * bool
  | Dloc of loc
  | Dfunval of efun
  | Dprocval of proc
  | Unbound
and mval =
  | Mint of int * taint
  | Mbool of bool * taint
  | Mstring of string * taint * bool
  | Undefined
;;





(* DEFINIZIONE TIPO PER PARSING *)
type parser = 
	| Lexp of exp 
	| Lcom of com
	| Ldecl of dic
	| Lparse of com
	| Lblock of block;;

let evaltomval e = match e with
  | Int (n,t) -> Mint (n,t)
  | Bool (n,t) -> Mbool (n,t)
  | String (n,t,c) -> Mstring (n,t,c)
  | _ -> raise Nonstorable

and mvaltoeval m = match m with
  | Mint (n,t) -> Int (n,t)
  | Mbool (n,t) -> Bool (n,t)
  | Mstring (n,t,c) -> String (n,t,c)
  | _ -> Novalue

and evaltodval e = match e with
  | Int (n,t) -> Dint (n,t)
  | Bool (n,t) -> Dbool (n,t)
  | String (n,t,c) -> Dstring (n,t,c)
  | Funval n -> Dfunval n
  | Novalue -> Unbound

and dvaltoeval e = match e with
  | Dint (n,t) -> Int (n,t)
  | Dbool (n,t) -> Bool (n,t)
  | Dstring (n,t,c) -> String (n,t,c)
  | Dfunval n -> Funval n
  | Dprocval n -> raise Nonexpressible
  | Dloc n -> raise Nonexpressible
  | Unbound -> Novalue;;

let op_and = function
  | (T,T) -> if(!safe_mode) then (print_string "\027[41m";failwith("\027[5m Unsafe operation!\027[0m"));T
  | (T,U) -> if(!safe_mode) then (print_string "\027[41m";failwith("\027[5m Unsafe operation!\027[0m"));T
  | (U,T) -> if(!safe_mode) then (print_string "\027[41m";failwith("\027[5m Unsafe operation!\027[0m"));T
  | (U,U) -> U

let check_safe = function
	| T -> if(!safe_mode) then (print_string "\027[41m";failwith("\027[5m Unsafe operation!\027[0m"));T
	| U -> U

(*--------------------------------IMPLEMENTAZIONE DELLE FUNZIONI SEMANTICHE------------------------------------------------*)		

let typecheck (x, y) =
  match x with
  | "bool" ->
     (match y with
     | Bool(u, w) -> true
     | _ -> false)
  | "int" ->
     (match y with
     | Int(u,w) -> true
     | _ -> false)
  | "string" ->
     (match y with
     | String(u,w,c) -> true
     | _ -> false)
  | _ -> failwith("not a valid type")
     
let et (x,y) =
  if typecheck("bool", x) && typecheck("bool", y) then
    (match (x,y) with
    | (Bool(u,w), Bool(z,k)) ->
       if u = false && z = true then 
	 Bool(false, check_safe(w))
       else if z = false && u = true then
	 Bool(false, check_safe(k))
       else
	 Bool((u && z), op_and (w,k))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and vel (x,y) =
  if typecheck("bool", x) && typecheck("bool", y) then
    (match (x,y) with
    | (Bool(u,w), Bool(z,k)) ->
       if u = false && z = true then
	 Bool(true, check_safe(k))
       else if u = true && z = false then
	 Bool(true, check_safe(w))
       else
	 Bool((u || z), op_and (w,k))	      
    | _ -> failwith ("type error"))
  else failwith ("type error")

and non x =
  if typecheck("bool", x) then
    (match x with
    | Bool(y,z) -> Bool((not y), check_safe(z))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and minus x =
  if typecheck("int", x) then
    (match x with
    | Int(y,z) -> Int(-y, check_safe(z))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and mult (x,y) =
  if typecheck("int", x) && typecheck("int", y) then
    (match (x,y) with
    | (Int(u,w), Int(z,k)) ->
       if u = 0 && z <> 0 then
	 Int(0, check_safe(w))
       else if z = 0 && u <> 0 then
	 Int(0, check_safe(k))
       else
	 Int((u * z), op_and (w,k))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and plus (x,y) =
  if typecheck("int", x) && typecheck("int", y) then
    (match (x,y) with
    | (Int(u,w), Int(z,k)) ->
       if u = 0 && z <> 0 then
	 Int(z, check_safe(k))
       else if z = 0 && u <> 0 then
	 Int(u, check_safe(w))
       else
	 Int((u + z), op_and (w,k))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and diff (x,y) =
  if typecheck("int", x) && typecheck("int", y) then
    (match (x,y) with
    | (Int(u,w), Int(z,k)) ->
       if u = 0 && z <> 0 then
	 Int(-z, check_safe(k))
       else if z = 0 && u <> 0 then
	 Int(u, check_safe(w))
       else
	 Int((u - z), op_and (w,k))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and eq (x,y) =
  if typecheck("int", x) && typecheck("int", y) then
    (match (x,y) with
    | (Int(u,w), Int(z,k)) -> Bool((u = z), op_and (w,k))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and zero x =
  if typecheck("int", x) then
    (match x with
    | Int(y,z) -> Bool((y = 0), check_safe(z))
    | _ -> failwith ("type error"))
  else failwith ("type error")

and len x =
  if typecheck("string", x) then
    (match x with
    | String(u,z,c) -> Int((String.length u), check_safe(z))
    | _ -> failwith("type error"))
  else failwith ("type error")

and sub (s,x,y) =
  if typecheck("string", s) && typecheck("int", x) && typecheck("int", y) then
    (match (s,x,y) with
    | (String(s,t1,c), Int(x,t2), Int(y,t3)) ->
       if x < y then
	 String((String.sub s (x-1) (y-x+1)), op_and (t1, op_and (t2,t3)),false)
       else if (x = y) then
	 String((String.sub s (x-1) 1), op_and (t1, op_and (t2,t3)),false)
       else
	 String("", T,false)
    | _ -> failwith("type error"))
  else failwith ("type error")

and conc (x,y) =
  if typecheck("string", x) && typecheck("string", y) then
    (match (x,y) with
    | (String(u,w,c1),String(z,k,c2)) ->
       if ((String.length u = 0) && (String.length z <> 0)) then
	 String(z,check_safe(k),false)
       else if ((String.length z = 0) && (String.length u <> 0)) then
	 String(u,check_safe(w),false)
       else
	 String((u ^ z), op_and (w,k),false)
    | _ -> failwith("type error"))
  else failwith ("type error")


(*--------------------------------FINE IMPLEMENTAZIONE DELLE FUNZIONI SEMANTICHE------------------------------------------------*)
