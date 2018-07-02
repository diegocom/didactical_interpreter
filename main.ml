
open Syntax;;
open Funenv;;
open Funstore;;
open Semantics;;
open Functions;;

(*FUNZIONI DI PARSING*)
(*Funzione che permette di parserizzare una riga*)
let parserRiga x =  let lexbuf = (Lexing.from_string(x^"\n")) in 
     let comandi = Parser.main Lexer.token lexbuf in if(!debug_mode) then print_parser comandi; comandi;;
(*Funzione che permette di parserizzare una lista di comandi e la esegue passandola alla semt*)
let rec parserLista x r s = match x with
  |[] -> (r,s)
  | h::t -> let (r,s) = (semt (parserRiga (h ^ ";;")) r s) in parserLista t r s;;


(*-----------------------INIZIO MAIN FILE CALC------------------------------------*)
(*DICHIARAZIONE DELLE ECCEZIONI*)
exception Parametri of string;;

(*LEGGO IL FILE PASSATO COME PARAMETRO*)
if(Array.length Sys.argv < 2) then (raise (Parametri "Passa il nome del programma da eseguire!"));;
let testo = ref "";;
let lista = ref [] ;;

let in_channel = open_in Sys.argv.(1) in
(try
  while true do
    let line = input_line in_channel in
    lista := line :: !lista;
  done
with End_of_file ->
  close_in in_channel);;


(*CREO AMBIENTE E STORE VUOTO DA CUI PARTIRE*)
let (rho1,store1) = ((emptyenv Unbound),(emptystore Undefined));;


(*UNISCO TUTTO IL TESTO LETTO DAL FILE*)
testo := unisci (List.rev !lista);;


(*SE E' ATTIVO IL DEBUG,STAMPO L'ELENCO DI ISTRUZIONI LETTO DAL FILE*)
if(!debug_mode) then
	(print_string("PROGRAMMA LETTO DAL FILE: \n");
	let rec elencoComandi = Str.split (Str.regexp ";;") !testo in
		stampa ( elencoComandi));;

print_string("\n\nESEGUO..\n\n");;


(*ESEGUO L'ELENCO DI COMANDI LETTO DAL FILE*)

let elencoComandi = Str.split (Str.regexp ";;") !testo in
	let (rho,store) = parserLista elencoComandi rho1 store1 in 
		if(!enable_print_env) then (print_string "\n\n\027[42m-----STATO AMBIENTE-----\027[0m\n\n";print_env !list_var rho store);
		if(!enable_print_store) then (print_string "\n\n\027[42m-----STATO MEMORIA-----\027[0m\n\n";print_store store 0);(rho,store);;


(*-----------------------FINE MAIN FILE CALC------------------------------------*)



