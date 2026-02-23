
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
  
  | TEbinop (op, e1, e2) ->
    compile_expr e2 ++ 
    pushq (reg rax) ++
    compile_expr e1 ++
    popq rbx ++
    (match op with 
    | Badd -> addq (reg rbx) (reg rax)
    | Bsub -> subq (reg rbx) (reg rax)
    | Bmul -> imulq (reg rbx) (reg rax)
    | Bdiv | Bmod ->
        cqto ++
        idivq (reg rbx) ++
        (if op = Bmod then movq (reg rdx) (reg rax) else nop)
    | Beq | Bne | Blt | Ble | Bgt | Bge ->
        let l_true = new_label () in
        let l_end = new_label () in
        cmpq (reg rbx) (reg rax) ++
        (match op with
        | Beq -> je l_true
        | Bne -> jne l_true
        | Blt -> jl l_true
        | Ble -> jle l_true
        | Bgt -> jg l_true
        | Bge -> jge l_true
        | _ -> assert false) ++
        movq (imm 0) (reg rax) ++
        jmp l_end ++
        label l_true ++
        movq (imm 0) (reg rax) ++
        jmp l_end ++
        label l_true ++
        movq (imm 1) (reg rax) ++
        label l_end
    |Band ->
      let l_false = new_label () in
      let l_end = new_label () in
      testq (reg rax) (reg rax) ++
      jz l_false ++
      testq (reg rbx) (reg rbx) ++
      jz l_false ++
      movq (imm 1) (reg rax) ++
      jmp l_end ++
      label l_false ++
      xorq (reg rax) (reg rax) ++
      label l_end
    |Bor ->
      let l_true = new_label () in
      let l_end = new_label () in
      testq (reg rax) (reg rax) ++
      jnz l_true ++
      testq (reg rbx) (reg rbx) ++
      jnz l_true ++
      xorq (reg rax) (reg rax) ++
      jmp l_end ++
      label l_true ++
      movq (imm 1) (reg rax) ++
      label l_end)

    | TEunop (op, e1) ->
      (match op with
      | Uneg ->
          compile_expr e1 ++
          negq (reg rax)
      | Unot ->
          compile_expr e1 ++
          testq (reg rax) (reg rax) ++
          setz (reg al) ++
          movzbq (reg al) rax
        | Uamp ->
          compile_lvalue e1 
        | Ustar ->
          compile_expr e1 ++
          movq (ind rax) rax)
      
  | TEdot (e1, f) ->
          compile_lvalue e ++
          movq (ind rax) rax
      
  | TEcall (fn, args) ->
      let nargs = List.length args in 
      let push_args = 
        List.fold_right (fun arg code ->
          compile_expr arg ++
          pushq (reg rax) ++
          code
        ) args nop
       in
      push_args ++
      call (fn.fn_name) ++ 
      (if nargs > 0 then 
        addq (imm (8 * nargs)) (reg rsp)
      else nop)
  
  | TEassign (lvs, rvs) ->
    (match lvs, rvs with
    | [lv], [rv] ->
        compile_expr rv ++
        pushq (reg rax) ++
        compile_lvalue lv ++
        popq rbx ++
        movq (reg rbx) (ind rax) ++
        xorq (reg rax) (reg rax)
    | _ ->
      let save_rvs = List.fold_right (fun rv code ->
        compile_expr rv ++ 
        pushq (reg rax) ++ 
        code
      ) rvs nop in 
      let do_assigns = List.fold_left (fun code lv ->
        code ++
        compile_lvalue lv ++
        popq rax ++
        movq (reg rax) (ind rbx)
    ) nop (List.rev lvs) in 
     save_rvs
     do_assigns ++
     xorq (reg rax) (reg rax))
     
  | TEvars vars ->
    xorq (reg rax) (reg rax) 
  
  | TEif (cond, e_then, e_else) ->
      let l_else = new_label () in 
      let l_end = new_label () in
      compile_expr cond ++
      testq (reg rax) (reg rax) ++
      jz l_else ++
      compile_expr e_then ++
      jmp l_end ++
      label l_else ++
      compile_expr e_else ++
      label l_end
  
  | TE

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
