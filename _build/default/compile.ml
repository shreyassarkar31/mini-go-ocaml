
(** Code generation for Mini Go programs (TODO) *)

open Format
open Ast
open Tast
open X86_64

let debug = ref false

let iter f = List.fold_left (fun code x -> code ++ f x) nop
let iter2 f = List.fold_left2 (fun code x y -> code ++ f x y) nop

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

let strings = Hashtbl.create 17
let add_string s =
  try Hashtbl.find strings s
  with Not_found ->
    let l = new_label () in
    Hashtbl.add strings s l;
    l

let rec sizeof = function 
  | Tint | Tbool | Tstring | Tptr _ | Tnil -> 8 
  | Tstruct s -> s.s_size 
  | Tmany _ -> 0 


let rec compile_lvalue e =
  match e.expr_desc with 
  | TEident v ->
      leaq (ind ~ofs:v.v_ofs rbp) rax 

  | TEdot (e1, f) ->
    compile_lvalue e1 ++
    (if f.f_ofs <> 0 then addq (imm f.f_ofs) (reg rax) else nop)
  
  | TEunop (Ustar, e1) ->
    compile_expr e1 

  | _ ->
    failwith "compile_lvalue: not an lvalue"

and compile_expr e =
  match e.expr_desc with 
  | TEskip ->
      nop

  | TEconstant c ->
      (match c with 
      | Cint n -> movq (imm n) rax 
      | Cbool b -> movq (imm (if b then 1 else 0)) (reg rax)
      | Cstring s -> 
          let l = add_string s in 
          movq (ilab l) (reg rax))
  | TEnil ->
      xorq (reg rax) (reg rax)
  | TEident v ->
      movq (ind ~ofs:v.v_ofs rbp) rax
  
  TEbinop (op, e1, e2) ->
    compile_expr e2 ++ 
    pushq (reg rax) ++
    compile_expr e1 ++
    popq rbx ++
    (match op with 
    | Badd -> addq (reg rbx) (reg rax)
    | Bsub -> subq (reg rbx) (reg rax)
    | Bmul -> imulq (reg rbx) (reg rax)
    | Bdiv | Bmod ->
        
    | _ -> failwith "compile_expr: unsupported binary operator")
let file ?debug:(b=false) (dl: Tast.tfile): X86_64.program =
  debug := b;
  { text =
      globl "main" ++ label "main" ++
      nop (* TODO call Go main function here *) ++
      xorq (reg rax) (reg rax) ++
      ret ++
      nop (* TODO assembly code for the Go functions here *) ++
      inline "
# TODO some auxiliary assembly functions, if needed
"
  ++ aligned_call_wrapper ~f:"malloc" ~newf:"malloc_"
  ++ aligned_call_wrapper ~f:"calloc" ~newf:"calloc_"
  ++ aligned_call_wrapper ~f:"printf" ~newf:"printf_"
;
    data =
      nop (* TODO static data here, such as string constants *)
    ;
  }
