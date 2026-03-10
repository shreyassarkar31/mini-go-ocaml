
(** Static type checking of Mini Go programs (TODO) *)

open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let fmt_imported = ref false 
let fmt_used = ref false


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

let rec string_of_type = function
  | Tint -> "int"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tnil -> "nil"
  | Tptr t -> "*" ^ string_of_type t
  | Tstruct s -> s.s_name
  | Tmany [] -> "void"
  | Tmany [t] -> string_of_type t
  | Tmany l -> "(" ^ String.concat ", " (List.map string_of_type l) ^ ")"

let rec type_type senv ptyp = 
  match ptyp with 
  | PTident id ->
    (match id.id with
    | "int" -> Tint
    | "bool" -> Tbool
    | "string" -> Tstring
    | s ->
    (try Tstruct (Hashtbl.find senv s)
     with Not_found -> errorm ~loc:id.loc "unknown type %s" s))
     | PTptr pt -> Tptr (type_type senv pt)
    
let rec type_expr env fenv senv ret_type e =
  let loc = e.pexpr_loc in 
  let desc, ty = match e.pexpr_desc with
    | PEskip -> TEskip, Tmany []
    | PEconstant c ->
      let ty = match c with 
        | Cint _ -> Tint
        | Cbool _ -> Tbool
        | Cstring _ -> Tstring
      in TEconstant c, ty

    | PEnil -> TEnil, Tnil

    | PEident id ->
      (try 
        let v = Hashtbl.find env id.id in
        v.v_used <- true;
        TEident v, v.v_typ
      with Not_found -> errorm ~loc "unknown variable %s" id.id)

    | PEbinop (op, e1, e2) ->
      let te1 = type_expr env fenv senv ret_type e1 in
      let te2 = type_expr env fenv senv ret_type e2 in
      let ty = match op with
        | Badd | Bsub | Bmul | Bdiv | Bmod ->
            if not (eq_type te1.expr_typ Tint && eq_type te2.expr_typ Tint) then
                errorm ~loc "arithmetic operators require int operands";
              Tint
        | Beq | Bne ->
            if not (compatible te1.expr_typ te2.expr_typ) then
                errorm ~loc "comparison requires compatible types";
              Tbool
        | Blt | Ble | Bgt | Bge ->
            if not (eq_type te1.expr_typ Tint && eq_type te2.expr_typ Tint) then
                errorm ~loc "comparison operators require int operands";
              Tbool
        | Band | Bor ->
            if not (eq_type te1.expr_typ Tbool && eq_type te2.expr_typ Tbool) then
                errorm ~loc "logical operators require bool operands";
              Tbool
        in
        TEbinop (op, te1, te2), ty
    
    | PEunop (op, e1) ->
      let te1 = type_expr env fenv senv ret_type e1 in
      let ty = match op with
        | Uneg ->
            if not (eq_type te1.expr_typ Tint) then
                errorm ~loc "unary minus requires int operand";
              Tint
        | Unot ->
            if not (eq_type te1.expr_typ Tbool) then
                errorm ~loc "logical not requires bool operand";
              Tbool
        | Uamp ->
            (match te1.expr_desc with
            | TEident v -> v.v_addr <- true 
            | TEdot _ -> ()
            | _ -> errorm ~loc "cannot take address of non-lvalue");
            Tptr te1.expr_typ     
        | Ustar ->
            (match te1.expr_typ with
            | Tptr t -> t
            | _ -> errorm ~loc "dereferencing requires pointer type")
      in
      TEunop (op, te1), ty

    | PEcall (f, args) ->
      if !debug then
        Format.eprintf "DEBUG: Calling function '%s'@." f.id;

      let is_print_fn =
        f.id = "fmt.Print" || f.id = "fmt.Println" in 

      let is_new = f.id = "new" in

      if is_print_fn then begin
        if not !fmt_imported then
          errorm ~loc:f.loc "cannot use fmt functions without importing \"fmt\"";
        fmt_used := true
      end;

      if is_print_fn then begin
        let targs = List.map (type_expr env fenv senv ret_type) args in
        List.iter (fun targ ->
          let check_printable ty =
            match ty with 
            | Tint | Tbool | Tstring | Tptr _ | Tnil -> ()
            | _ -> errorm ~loc:f.loc "print only accepts int, bool, string, or pointer"
          in
          match targ.expr_typ with 
          | Tmany l -> List.iter check_printable l
          | t -> check_printable t
          ) targs;
          if f.id = "fmt.Println" then
            let newline_expr = {
              expr_desc = TEconstant (Cstring "\n");
              expr_typ = Tstring;
            } in
          TEprint (targs @ [newline_expr]), Tmany []
          else
            TEprint targs, Tmany []
      end 
    else if is_new then begin
      match args with
      | [{pexpr_desc = PEident type_id; _}] ->
        let ty = type_type senv (PTident type_id) in
        TEnew ty, Tptr ty
      | _ -> errorm ~loc:f.loc "new expects exactly one type argument"
    end else begin
      (try
        let fn = Hashtbl.find fenv f.id in
        let targs = List.map (type_expr env fenv senv ret_type) args in

        let arg_types = 
          List.flatten (List.map (fun targ ->
            match targ.expr_typ with
            | Tmany l -> l 
            | t -> [t]
          ) targs) in

          let param_types = List.map (fun v -> v.v_typ) fn.fn_params in
          if List.length arg_types <> List.length param_types then
            errorm ~loc "function %s expects %d arguments but got %d" 
            f.id (List.length param_types) (List.length arg_types);
          List.iter2 (fun arg_t param_t ->
            if not (compatible arg_t param_t) then
              errorm ~loc "type mismatch in argument to %s: expected %s" 
              f.id (string_of_type param_t)
            ) arg_types param_types;
            let ret_ty = match fn.fn_typ with 
              | [] -> Tmany []
              | [t] -> t
              | l -> Tmany l
            in
            TEcall (fn, targs), ret_ty
        with Not_found -> 
          errorm ~loc:f.loc "unknown function %s" f.id)
      end
    | PEdot (e1, field) ->
      let te1 = type_expr env fenv senv ret_type e1 in 
      let s = match te1.expr_typ with 
        | Tstruct s -> s
        | Tptr (Tstruct s) -> s
        | _ -> errorm ~loc "dot operator requires struct type"
      in
      (try 
        let f = Hashtbl.find s.s_fields field.id in 
        TEdot (te1, f), f.f_typ
      with Not_found -> errorm ~loc:field.loc "struct %s has no field %s" s.s_name field.id)
      
    |PEassign (lvals, rvals) ->
      let tlvals = List.map (type_expr env fenv senv ret_type) lvals in
      let trvals = List.map (type_expr env fenv senv ret_type) rvals in
      
      List.iter (fun lv ->
        match lv.expr_desc with 
        | TEident _ | TEdot _ | TEunop (Ustar, _) -> ()
        | _ -> errorm ~loc "left hand side assigment must be an lvalue"
        ) tlvals;

        let rval_types = match trvals with 
          | [rv] ->
            (match rv.expr_typ with 
            | Tmany l -> l
            | t -> [t])
          | _ -> List.map (fun rv -> rv.expr_typ) trvals 
          in 

          if List.length tlvals <> List.length rval_types then
            errorm ~loc "assigment count mismatch: %d = %d" 
            (List.length tlvals) (List.length rval_types);

          List.iter2 (fun lv rt ->
            if not (compatible rt lv.expr_typ) then
              errorm ~loc "cannot assign %s to %s" 
              (string_of_type rt) (string_of_type lv.expr_typ)
            ) tlvals rval_types;

          TEassign (tlvals, trvals), Tmany []
        
    | PEvars (ids, ptyp_opt, exprs) ->
      
        if exprs = [] then begin
          (match ptyp_opt with
          | None -> errorm ~loc "type required when not initialising"
          | Some pt ->
            let declared_ty = type_type senv pt in
            let vars = List.map (fun id ->
              let v = new_var id.id id.loc declared_ty in
              if Hashtbl.mem env id.id then
                errorm ~loc:id.loc "variable %s already declared" id.id;
              Hashtbl.add env id.id v;
              v
            ) ids in
            TEvars vars, Tmany [])
        end else begin
          let texps = List.map (type_expr env fenv senv ret_type) exprs in

      let exp_types = match texps with 
        | [e] ->
          (match e.expr_typ with 
          | Tmany l -> l
          | t -> [t])
        | _ -> List.map (fun e -> e.expr_typ) texps 
        in

        if List.length ids <> List.length exp_types then
          errorm ~loc "variable declaration count mismatch: %d = %d" 
          (List.length ids) (List.length exp_types);

        let vars = List.map2 (fun id ty ->
          let v_ty = match ptyp_opt with 
            | Some pt ->
              let declared_ty = type_type senv pt in
              if not (compatible ty declared_ty) then
                errorm ~loc:id.loc "cannot use type %s as type %s"
                (string_of_type ty) (string_of_type declared_ty);
              declared_ty
            | None ->
              if ty = Tnil then
                errorm ~loc:id.loc "use of untyped nil";
                 ty
          in
          let v = new_var id.id id.loc v_ty in 
          if Hashtbl.mem env id.id then 
            errorm ~loc:id.loc "variable %s already declared" id.id;
          Hashtbl.add env id.id v;
          v
        ) ids exp_types in 

        let decl_expr = {
          expr_desc = TEvars vars;
          expr_typ = Tmany []
        } in

        let lvals = List.map (fun v -> {
          expr_desc = TEident v;
          expr_typ = v.v_typ
        }) vars in

        let assign_expr = {
          expr_desc = TEassign (lvals, texps);
          expr_typ = Tmany []
        } in
        TEblock [decl_expr; assign_expr], Tmany []
      end

    | PEif (cond, then_e, else_e) ->
      let te = type_expr env fenv senv ret_type cond in 
      if not (eq_type te.expr_typ Tbool) then
        errorm ~loc "condition of if must be bool";
      let tthen = type_expr env fenv senv ret_type then_e in 
      let telse = type_expr env fenv senv ret_type else_e in
      TEif (te, tthen, telse), Tmany[] 

    | PEreturn (exprs) ->
      let texprs = List.map (type_expr env fenv senv ret_type) exprs in 
      let expr_types = match texprs with 
      | [e] when (match e.expr_typ with Tmany _ -> true | _ -> false) ->
        (match e.expr_typ with 
        | Tmany l -> l
        | t -> [t])
      | _ -> List.map (fun e -> e.expr_typ) texprs
        in

      (match ret_type with
      | Tmany expected ->
        if List.length expr_types <> List.length expected then
          errorm ~loc "wrong number of return values";
        List.iter2 (fun et rt ->
          if not (compatible et rt) then
            errorm ~loc "return type mismatch"
        ) expr_types expected
      | expected ->
        if List.length expr_types <> 1 then 
          errorm ~loc "wrong number of return values";
        if not (compatible (List.hd expr_types) expected) then
          errorm ~loc "return type mismatch");
        TEreturn texprs, Tmany []
    
    |PEblock exprs -> 
      let texprs = List.map (type_expr env fenv senv ret_type) exprs in 
      TEblock texprs,Tmany []

    |PEfor (cond, body) ->
      let tcond = type_expr env fenv senv ret_type cond in
      if not (eq_type tcond.expr_typ Tbool) then
        errorm ~loc "condition of for must be bool";
      let tbody = type_expr env fenv senv ret_type body in
      TEfor (tcond, tbody), Tmany []
    
    |PEincdec (e1, op) ->
      let te1 = type_expr env fenv senv ret_type e1 in 
      if not (eq_type te1.expr_typ Tint) then
        errorm ~loc "increment/decrement requires int operand";
      (match te1.expr_desc with 
      | TEident _ | TEdot _ | TEunop (Ustar, _)  -> ()
      | _ -> errorm ~loc "increment/decrement requires lvalue");
      TEincdec (te1, op), Tmany []
  in
  { expr_desc = desc; expr_typ = ty }


let type_decl env fenv senv d =
  match d with
  | PDfunction pf ->
    let fn = try Hashtbl.find fenv pf.pf_name.id
               with Not_found -> 
                 errorm ~loc:pf.pf_name.loc "internal error: function not in environment" in
      
      
      let env' = empty_env () in
      List.iter (fun v -> Hashtbl.add env' v.v_name v) fn.fn_params;
      
      let ret_type = match fn.fn_typ with
        | [] -> Tmany []
        | [t] -> t
        | l -> Tmany l
      in
      let tbody = type_expr env' fenv senv ret_type pf.pf_body in

      let rec ends_with_return e =
        match e.expr_desc with
        | TEreturn _ -> true
        | TEblock [] -> false
        | TEblock exprs -> ends_with_return (List.hd (List.rev exprs))
        | TEif (_, e1, e2) -> ends_with_return e1 && ends_with_return e2
        | _ -> false
      in

      if fn.fn_typ <> [] && not (ends_with_return tbody) then
        errorm ~loc:pf.pf_name.loc "missing return at end of function";
      List.iter (fun v ->
        if not v.v_used && not (String.starts_with ~prefix:"_" v.v_name) then
          if !debug then
            Format.eprintf "Warning: parameter %s is unused@." v.v_name
      ) fn.fn_params;
      
      
      Hashtbl.iter (fun name v ->
        if not v.v_used && not (String.starts_with ~prefix:"_" v.v_name) then
          errorm ~loc:v.v_loc "%s declared but not used" v.v_name
      ) env';
      
      Some (TDfunction (fn, tbody))
  
  | PDstruct ps ->
      
      let s = try Hashtbl.find senv ps.ps_name.id 
            with Not_found ->
              errorm ~loc:ps.ps_name.loc "internal error: struct not in environment" in
      
    
      let offset = ref 0 in
      let fields = List.map (fun (id, ptyp) ->
        let ty = type_type senv ptyp in
        if Hashtbl.mem s.s_fields id.id then
          errorm ~loc:id.loc "duplicate field %s in struct %s" id.id s.s_name;

        (match ty with
        | Tstruct s2 when s2.s_name = s.s_name ->
            errorm ~loc:id.loc "invalid recursive type %s" s.s_name
        | _ -> ());

        let f = { f_name = id.id; f_typ = ty; f_ofs = !offset } in
        Hashtbl.add s.s_fields id.id f;
        offset := !offset + 8;
        f
      ) ps.ps_fields in
      
      s.s_list <- fields;
      s.s_size <- !offset;
      
      Some (TDstruct s)

let file ~debug:b (imp, dl : Ast.pfile) : Tast.tfile =
  debug := b;
  
  fmt_imported := imp;
  fmt_used := false;

  let senv = empty_senv () in
  let fenv = empty_fenv () in
  let env = empty_env () in
  
  List.iter (fun d ->
    match d with
    | PDstruct ps ->
        let s = {
          s_name = ps.ps_name.id;
          s_fields = Hashtbl.create 17;
          s_list = [];
          s_size = 0;
        } in
        if Hashtbl.mem senv ps.ps_name.id then
          errorm ~loc:ps.ps_name.loc "struct %s already declared" ps.ps_name.id;
        Hashtbl.add senv ps.ps_name.id s
    | PDfunction pf ->
        let params = List.map (fun (id, ptyp) ->
          let ty = type_type senv ptyp in
          new_var id.id id.loc ty
        ) pf.pf_params in
        let ret_types = List.map (type_type senv) pf.pf_typ in
        let fn = {
          fn_name = pf.pf_name.id;
          fn_params = params;
          fn_typ = ret_types;
        } in
        if Hashtbl.mem fenv pf.pf_name.id then
          errorm ~loc:pf.pf_name.loc "function %s already declared" pf.pf_name.id;
        Hashtbl.add fenv pf.pf_name.id fn
  ) dl;
  
  
  let tdecls = List.filter_map (type_decl env fenv senv) dl in

  let rec check_struct_cycle s path =
    let rec contains_cycle ty visited =
      match ty with
      | Tstruct s2 ->
        if List.mem s2.s_name visited then
          errorm "invalid recursive type: cycle involving %s" s2.s_name
        else
          List.iter (fun f ->
            contains_cycle f.f_typ (s2.s_name :: visited)
          ) s2.s_list
        | Tptr _ -> ()
        | _ -> ()
    in
    List.iter(fun f -> contains_cycle f.f_typ [s.s_name]) s.s_list
  in

  List.iter (function
  | TDstruct s -> check_struct_cycle s []
  | _ -> ()
  ) tdecls;

  let has_functions = List.exists (function TDfunction _ -> true | _ -> false) tdecls in 

  if has_functions then
    (try
      let main = Hashtbl.find fenv "main" in
      if main.fn_params <> [] then
        errorm "main function must have no parameters";
      if main.fn_typ <> [] then
        errorm "main function must have no return values"
    with Not_found ->
      errorm "missing main function");

  
  if !fmt_imported && not !fmt_used then
    errorm "imported and not used: \"fmt\"";
  
  
  
  tdecls
              



