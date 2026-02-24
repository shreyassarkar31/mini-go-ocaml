
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
      | Cint n -> movq (imm64 n) (reg rax)
      | Cbool b -> movq (imm (if b then 1 else 0)) (reg rax)
      | Cstring s -> 
          let l = add_string s in 
          movq (ilab l) (reg rax))
  | TEnil ->
      xorq (reg rax) (reg rax)
  | TEident v ->
      movq (ind ~ofs:v.v_ofs rbp) (reg rax)
  
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
          movq (ind rax) (reg rax))
      
  | TEdot (e1, f) ->
          compile_lvalue e1 ++
          movq (ind rax) (reg rax)
      
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
     save_rvs ++
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
  
  | TEreturn exprs ->
      (match exprs with 
      | [] -> xorq (reg rax) (reg rax)
      | [e] -> compile_expr e
      | e :: _ -> compile_expr e) ++
      movq (reg rbp) (reg rsp) ++ 
      popq rbp ++
      ret 
  
  | TEblock exprs ->
      iter compile_expr exprs

  | TEfor (cond, body) ->
      let l_start = new_label () in  
      let l_test = new_label () in 
      jmp l_test ++
      label l_start ++
      compile_expr body ++
      label l_test ++ 
      compile_expr cond ++
      testq (reg rax) (reg rax) ++
      jnz l_start

  | TEprint exprs ->
      iter (fun e ->
        match e.expr_typ with
        | Tint ->
            compile_expr e ++ 
            movq (reg rax) (reg rsi) ++
            movq (ilab "S_fmt_int") (reg rdi) ++ 
            xorq (reg rax) (reg rax) ++
            call "printf_"
        | Tbool ->
            let l_true = new_label () in
            let l_end = new_label () in
            compile_expr e ++
            testq (reg rax) (reg rax) ++ 
            jnz l_true ++ 
            movq (ilab "S_false") (reg rdi) ++ 
            jmp l_end ++
            label l_true ++
            movq (ilab "S_true") (reg rdi) ++
            label l_end ++
            xorq (reg rax) (reg rax) ++
            call "printf_"
        | Tstring ->
            compile_expr e ++
            movq (reg rax) (reg rdi) ++
            xorq (reg rax) (reg rax) ++
            call "printf_"
        | _ -> nop 
      
      ) exprs 

  | TEincdec (e1, op) ->
      compile_lvalue e1 ++
      movq (reg rax) (reg rbx) ++
      movq (ind rbx) (reg rax) ++ 
      (match op with
      | Inc -> incq (reg rax)
      | Dec -> decq (reg rax)) ++
      movq (reg rax) (ind rbx) ++
      xorq (reg rax) (reg rax)
      
  | TEnew ty ->
      movq (imm (sizeof ty)) (reg rdi) ++ 
      call "malloc_"
  

let compute_locals fn body =
  let locals = Hashtbl.create 17 in 
  let offset = ref (-8) in 

  let rec collect_vars e =
    match e.expr_desc with 
    | TEvars vars ->
        List.iter (fun v ->
          if not (Hashtbl.mem locals v.v_id) then begin 
            v.v_ofs <- !offset;
            offset := !offset - 8;
            Hashtbl.add locals v.v_id v 
          end
        ) vars
    | TEblock exprs -> List.iter collect_vars exprs
    | TEif (_, e1, e2)  -> collect_vars e1; collect_vars e2
    | TEfor (_, body) -> collect_vars body
    | TEassign (lvs, rvs) ->
        List.iter collect_vars lvs; List.iter collect_vars rvs
    | TEbinop (_, e1, e2) -> collect_vars e1; collect_vars e2
    | TEunop (_, e1) -> collect_vars e1
    | TEdot (e1, _) -> collect_vars e1
    | TEcall (_, args) -> List.iter collect_vars args
    | TEprint exprs -> List.iter collect_vars exprs
    | TEincdec (e1, _) -> collect_vars e1
    | TEreturn exprs -> List.iter collect_vars exprs
    | TEnew _ | TEident _ | TEconstant _ | TEnil | TEskip -> ()
in

let param_offset = ref 16 in 
  List.iter (fun v ->
    v.v_ofs <- !param_offset;
    param_offset := !param_offset + 8
  ) fn.fn_params;

  collect_vars body;

  -(!offset)

let compile_function fn body =
  let stack_size = compute_locals fn body in 
  let stack_size = (stack_size +15) land (-16) in 

  label fn.fn_name ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  (if stack_size > 0 then subq (imm stack_size) (reg rsp) else nop) ++
  compile_expr body ++
  movq (reg rbp) (reg rsp) ++
  popq rbp ++
  ret

  let compile_decl = function
    | TDfunction (fn, body) ->
        compile_function fn body
    | TDstruct _ -> nop

let file ?debug:(b=false) (dl: Tast.tfile): X86_64.program =
  debug := b;


  Hashtbl.clear strings;

  let text_code = iter compile_decl dl in 

  let has_main = List.exists (function 
    | TDfunction (fn, _) -> fn.fn_name = "main"
    | _ -> false
  ) dl in

  let main_wrapper = 
    if has_main then 
      globl "main" ++ 
      label "main" ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++
      call "main" ++
      xorq (reg rax) (reg rax) ++
      popq rbp ++
      ret
    else
      globl "main" ++
      label "main" ++
      xorq (reg rax) (reg rax) ++
      ret
  in

  let string_data = 
    Hashtbl.fold (fun s l code ->
      code ++
      label l ++
      string s
      ) strings nop
  in

  let format_strings =
    label "S_fmt_int" ++
    string "%ld" ++
    label "S_true" ++
    string "true" ++
    label "S_false" ++
    string "false"
in


{ text =
      main_wrapper ++
      text_code ++
      inline "\n# Auxiliary assembly functions\n" ++
      aligned_call_wrapper ~f:"malloc" ~newf:"malloc_" ++
      aligned_call_wrapper ~f:"calloc" ~newf:"calloc_" ++
      aligned_call_wrapper ~f:"printf" ~newf:"printf_"
  ;
    data =
      format_strings ++
      string_data
  }
