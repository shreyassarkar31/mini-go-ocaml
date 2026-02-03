
(** Static type checking of Mini Go programs (TODO) *)

open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string


(** use this function to report errors; it is a printf-like function, eg.

    errorm ~loc "bad arity %d for function %s" n f

*)
let errorm ?(loc=dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(** use this function to create variable, so that they all have a
    unique id if field `v_id` *)
let new_var : string -> Ast.location -> typ -> var =
  let id = ref 0 in
  fun x loc ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty;
      v_used = false; v_addr = false; v_ofs= -1 }

type env = (string, var) Hashtbl.t
let empty_env () : env = Hashtbl.create 17
let copy_env (e:env) : env =
  Hashtbl.fold (fun k v acc -> Hashtbl.add acc k v; acc) e (empty_env ())

type fenv = (string, function_) Hashtbl.t 
let empty_fenv () : fenv = Hashtbl.create 17 

type senv = (string, structure) Hashtbl.t 
let empty_senv () : senv = Hashtbl.create 17 

let rec eq_type t1 t2 = match t1, t2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring | Tnil, Tnil -> true
  | Tptr t1, Tptr t2 -> eq_type t1 t2 
  | Tstruct s1, Tstruct s2 -> s1.s_name = s2.s_name 
  | Tmany l1, Tmany l2 ->
    List.length l1 = List.length l2 && List.for_all2 eq_type l1 l2 
  | Tnil, Tptr _ | Tptr _, Tnil -> true 
  | _ -> false

let compatible t1 t2 = match t1, t2 with 
    | Tnil, Tptr _ | Tptr _, Tnil -> true 
    | _ -> eq_type t1 t2 





let file ~debug:b (imp, dl : Ast.pfile) : Tast.tfile =
  debug := b;
  failwith "Not implemented"



