open Prelude

module E = Exprjs_syntax
module S = Ljs_syntax
module F = Env_free_vars
open Ljs_annotation

let idx = ref 0

let mk_id str = 
  idx := !idx + 1;
  "%" ^ str ^ (string_of_int !idx)

let eq p e1 e2 = S.Op2 (p, "stx=", e1, e2)

let undef_test p v = eq p v (S.Undefined p)

let null_test p v = eq p v (S.Null p)

let type_test p v typ =
  eq p (S.Op1 (p, "typeof", v)) (S.String (p, typ, [Top]))

let is_object_type p o = S.App (p, F.env_var p "%IsObject", [o])

let throw_typ_error p msg =
  S.App (p, F.env_var p "%TypeError", [S.String (p, msg, [Top])])

let make_get_field p obj fld =
  let argsobj = S.Object (p, S.d_attrs, [], [Top]) in
  S.GetField (p, obj, fld, argsobj)

let get_string_field p obj str =
  make_get_field p obj (S.String (p, str, [Top]))

let to_string p v =
  match v with
    | S.String (p, s, _) -> S.String (p, s, [Top])
    | _ -> S.App (p, F.env_var p "%ToString", [v])

let to_object p v =
  match v with
    | S.Id (p, "%context") -> v
    | _ -> S.App (p, F.env_var p "%ToObject", [v])

let with_error_dispatch p e =
  S.TryCatch (p, e, S.Id (p, "%ErrorDispatch"))

let prop_accessor_check p v =
  match v with
    | S.Id (p, "%context") -> v
    | _ -> S.App (p, F.env_var p "%PropAccessorCheck", [v])

let ljs_bool b = if b then S.True (Pos.dummy, [Top]) else S.False (Pos.dummy, [Top])

(* 15.4: A property name P (in the form of a String value) is an array index if and
 * only if ToString(ToUint32(P)) is equal to P and ToUint32(P) is not equal to
 * 2^32−1.  This is checked in %EnvCheckAssign *)
let make_set_field p obj fld value =
  match obj with
  | S.Id (p, "%context") -> 
    S.App (p, F.env_var p "%EnvCheckAssign", [obj; fld; value; S.Id (p, "#strict")])
  | _ -> with_error_dispatch p
      (S.App (p, F.env_var p "%set-property", [obj; fld; value]))

let make_args_obj p is_new args =
    let n_args = List.length args in
    let indices = Prelude.iota n_args in
    let combined = List.combine indices args in
    let records =
      List.map (fun (n, arg) -> (n, {S.value = arg; S.writable = true})) combined in
    let props = 
      List.map 
        (fun (n, rcrd) -> (string_of_int n, S.Data (rcrd, true, true))) records in
    let arg_constructor = if is_new then "%mkNewArgsObj" else "%mkArgsObj" in
    S.App (p, S.Id (p, arg_constructor), [S.Object (p, S.d_attrs, props, [Top])])

let rec ejs_to_ljs (e : E.expr) : S.exp = match e with
  | E.True (p) -> S.True (p, [Top])
  | E.False (p) -> S.False (p, [Top])
  | E.Num (p, n) -> S.Num (p, n, [Top])
  | E.Undefined (p) -> S.Undefined (p)
  | E.Null (p) -> S.Null (p)
  | E.String (p, s) -> S.String (p, s, [Top])
  | E.ArrayExpr (p, el) ->
    let rec e_to_p e n = (
      string_of_int n, 
      S.Data ({ S.value = e; S.writable = true; }, true, true))
    and el_to_pl l n = match l with
      | [] -> []
      | first :: rest -> (e_to_p first n) :: el_to_pl rest (n + 1) in
    let desugared = List.map ejs_to_ljs el
    and a_attrs = {
      S.primval = None;
        S.code = None;
        S.proto = Some (F.env_var p "%ArrayProto"); 
        S.klass = "Array";
        S.extensible = true; } in
    let exp_props = el_to_pl desugared 0 in
    let lfloat = float_of_int (List.length exp_props) in
    let l_prop = (* TODO: is array length prop writ/enum/configurable? *)
      S.Data (
        { S.value = S.Num (p, lfloat, [Top]); S.writable = true; },
        false,
        false) in
    S.Object (p, a_attrs, ("length", l_prop) :: exp_props, [Top])
  | E.ObjectExpr (p, pl) ->
    (* Given a tuple, if it's a getter/setter, create a name-accessor pair and add to
     * sofar *)
    let add_accessor pr sofar = match pr with
      | (_, _, E.Getter (nm, exp)) ->
        let gval = get_fobj p [] exp (S.Id (p, "%context")) in
        let a = { S.getter = gval; S.setter = S.Undefined (p); } in
        (nm, S.Accessor (a, false, false)) :: sofar
      | (_, _, E.Setter (nm, exp)) ->
        let (param_name, sfunc) = match exp with
          | E.LetExpr (_, nm, _, body) -> (nm, body)
          | _ -> failwith "setter desugaring error: expected LetExpr here" in
        let sval = get_fobj p [param_name] sfunc (S.Id (p, "%context")) in
        let a = { S.getter = S.Undefined (p); S.setter = sval; } in
        (nm, S.Accessor (a, false, false)) :: sofar
      | _ -> sofar in
    (* Given a list of tuples, produce a list of name, accessor pairs *)
    let rec accessors tl sofar = match tl with
      | [] -> sofar
      | t :: rest -> accessors rest (add_accessor t sofar) in
    (* Get only those pairs with name = nm *)
    let tuples tl nm = List.filter (fun (n, _) -> n = nm) tl in
    (* Given a list of name-accessor pairs, reduce them to one *)
    let rec reduce al result = match al with
      | [] -> result
      | (nm, S.Accessor (a, wr, cfg)) :: rest ->
        let result_a = match result with
          | S.Accessor (aa, _, _) -> aa
          | _ -> failwith "Fatal: non-accessor in exprjs_to_ljs.reduce" in
        let next = match a with
          | { S.getter = S.Undefined _; S.setter = s; } ->
            S.Accessor ({ S.getter = result_a.S.getter; S.setter = s; }, wr, cfg)
          | { S.getter = g; S.setter = S.Undefined _; } ->
            S.Accessor ({ S.getter = g; S.setter = result_a.S.setter; }, wr, cfg)
          | _ -> S.Accessor (a, wr, cfg) in
        reduce rest next
      | _ -> failwith "Fatal: exprjs_to_ljs.reduce given non-accessors" in
    let dup_pairs = accessors pl [] in
    let name_lst = remove_dupes (map (fun (n, _) -> n) dup_pairs) in
    let name_assoc = map (fun n -> (n, tuples dup_pairs n)) name_lst in
    let dummy_prop = 
      S.Accessor (
        { S.getter = S.Undefined (p); S.setter = S.Undefined (p); }, true, true) in
    let reduced_assoc = map (fun (n, al) -> (n, reduce al dummy_prop)) name_assoc in
    let data_props = 
      List.filter (fun p -> let result = 
        match p with (_, _, E.Data _) -> true | _ -> false in result) pl in
    let rec ejsprop_to_sprop pr = match pr with
      | E.Data (e) -> 
          let rec v = ejs_to_ljs e
          and d = { S.value = v; S.writable = true; } in
          S.Data (d, true, true)
      | _ -> failwith "accessor properties should have been filtered out"
    and tuple_to_prop t = match t with
      (p, s, pr) -> (s, ejsprop_to_sprop pr)
    and form_props props = match props with
      | [] -> []
      | first :: rest -> (tuple_to_prop first) :: form_props rest in
    let data_result = form_props data_props in
    let o_attrs = {
      S.primval = None;
      S.code = None;
      S.proto = Some (F.env_var p "%ObjectProto");
      S.klass = "Object";
      S.extensible = true; } in
    S.Object (p, o_attrs, List.append reduced_assoc data_result, [Top])
  | E.ThisExpr (p) -> F.env_var p "%this"
  | E.IdExpr (p, nm) -> S.Id (p, nm)
  | E.BracketExpr (p, l, r) -> 
    let o = prop_accessor_check p (ejs_to_ljs l) in
    let f = to_string p (ejs_to_ljs r) in
    make_get_field p o f
  | E.NewExpr (p, econstr, eargs) -> 
    let constr_id = mk_id "constr" in
    let pr_id = mk_id "cproto" in
    let newobj = mk_id "newobj" in
    let constr_result = mk_id "constr_ret" in
    let getterargs = S.Object (p, S.d_attrs, [], [Top]) in
    let constrargs = make_args_obj p true (map ejs_to_ljs eargs) in
    S.Let (p, constr_id, ejs_to_ljs econstr,
      S.If (p, S.Op1 (p, "!", type_test p (S.Id (p, constr_id)) "function"),
        throw_typ_error p "Constructor was not a function", 
        S.Let (p, pr_id, S.GetField (p, S.Id (p, constr_id), 
                           S.String (p, "prototype", [Top]), getterargs),
          S.Let (p, pr_id, S.If (p, is_object_type p (S.Id (p, pr_id)),
                                    S.Id (p, pr_id),
                                    F.env_var p "%ObjectProto"),
            S.Seq (p,
              S.If (p, S.Op1 (p, "!", is_object_type p (S.Id (p, pr_id))),
                S.SetBang (p, pr_id, F.env_var p "%ObjectProto"), S.Undefined p),
            S.Let (p, newobj, S.Object (p, { S.d_attrs with S.proto = Some (S.Id (p, pr_id)) }, [], [Top]),
              S.If (p, null_test p (S.GetObjAttr (p, S.Code, S.Id (p, constr_id))),
                    throw_typ_error p "Constructor was not applicable",
                    S.Let (p, constr_result, S.App (p, S.Id (p, constr_id), [S.Id (p, newobj); constrargs]),
                    S.If (p, is_object_type p (S.Id (p, constr_result)),
                      S.Id (p, constr_result),
                      S.Id (p, newobj))))))))))
  | E.PrefixExpr (p, op, exp) -> let result = match op with
    | "postfix:++" -> let target = ejs_to_ljs exp in
      begin match target with
        | S.GetField (_, obj, fld, _) ->
          S.App (p, F.env_var p "%PostIncrement", [obj; fld])
        | _ -> failwith "desugaring error: postfix:++"
      end
    | "postfix:--" -> let target = ejs_to_ljs exp in
      begin match target with
        | S.GetField (_, obj, fld, _) ->
          S.App (p, F.env_var p "%PostDecrement", [obj; fld])
        | _ -> failwith "desugaring error: postfix:--"
      end
    | "prefix:++" -> let target = ejs_to_ljs exp in 
      begin match target with
        | S.GetField (_, obj, fld, _) ->
          S.App (p, F.env_var p "%PrefixIncrement", [obj; fld])
        | _ -> failwith "desugaring error: prefix:++"
      end
    | "prefix:--" -> let target = ejs_to_ljs exp in
      begin match target with
        | S.GetField (_, obj, fld, _) ->
          S.App (p, F.env_var p "%PrefixDecrement", [obj; fld])
        | _ -> failwith "desugaring error: prefix:--"
      end
    | "typeof" -> let target = ejs_to_ljs exp in
      begin match target with
        | S.GetField (_, (S.Id (_, "%context") as context), fldexpr, _) ->
          S.App (p, F.env_var p "%Typeof", [context; fldexpr])
        | _ -> S.Op1 (p, "typeof", target)
      end
    | "delete" -> let result = match exp with
      | E.BracketExpr (pp, obj, fld) -> 
        let fld_str = to_string p (ejs_to_ljs fld)
        and sobj = ejs_to_ljs obj in
        begin match sobj with
          | S.Id (_, "%context") ->
            let null = S.Null (p) in
            S.App (pp, F.env_var p "%ThrowSyntaxError", [null; null])
          | _ ->
            with_error_dispatch p (S.DeleteField (pp, sobj, fld_str))
        end
      | _ -> S.True (p, [Top]) in result
    | "-" -> S.App(p, F.env_var p "%UnaryNeg", [ejs_to_ljs exp])
    | "+" -> S.App (p, F.env_var p "%UnaryPlus", [ejs_to_ljs exp])
    | "~" -> S.App (p, F.env_var p "%BitwiseNot", [ejs_to_ljs exp])
    | _ -> S.Op1 (p, op, ejs_to_ljs exp) in result
  | E.InfixExpr (p, op, l, r) ->
    let aid = mk_id "a" in
    let bid = mk_id "b" in
    let op_func =
      S.Lambda (p, [aid; bid],
        S.Op2 (p, op, S.Id (p, aid), S.Id (p, bid))) in
    let sl = ejs_to_ljs l and sr = ejs_to_ljs r in
    begin match op with
      | "&&" ->
        let lid = mk_id "l-evaled" in
        S.Let (p, lid, sl,
          S.If (p, 
            S.App (p, F.env_var p "%ToBoolean", [S.Id (p, lid)]),
            sr, 
            S.Id (p, lid)))
      | "||" ->
        let lid = mk_id "l-evaled" in
        S.Let (p, lid, sl,
          S.If (p,
            S.App (p, F.env_var p "%ToBoolean", [S.Id (p, lid)]),
            S.Id (p, lid),
            sr))
      | "!==" -> S.Op1 (p, "!", S.Op2 (p, "stx=", sl, sr))
      | "!=" -> S.Op1 (p, "!", S.App (p, S.Id (p, "%EqEq"), [sl; sr]))
      | "==" -> S.App (p, F.env_var p "%EqEq", [sl; sr])
      | "+" -> S.App (p, F.env_var p "%PrimAdd", [sl; sr])
      | "-" -> S.App (p, F.env_var p "%PrimSub", [sl; sr])
      | "<<" -> S.App (p, F.env_var p "%LeftShift", [sl; sr])
      | ">>" -> S.App (p, F.env_var p "%SignedRightShift", [sl; sr])
      | ">>>" -> S.App (p, F.env_var p "%UnsignedRightShift", [sl; sr])
      | "&" | "^" | "|" -> 
        S.App (p, F.env_var p "%BitwiseInfix", [sl; sr; op_func])
      | "*" | "%" | "/" -> 
        S.App (p, F.env_var p "%PrimMultOp", [sl; sr; op_func])
      | "<" -> S.App (p, F.env_var p "%LessThan", [sl; sr])
      | ">" -> S.App (p, F.env_var p "%GreaterThan", [sl; sr])
      | "<=" -> S.App (p, F.env_var p "%LessEqual", [sl; sr])
      | ">=" -> S.App (p, F.env_var p "%GreaterEqual", [sl; sr])
      | "instanceof" -> S.App (p, F.env_var p "%instanceof", [sl; sr])
      | "in" -> S.App (p, F.env_var p "%in", [sl; sr])
      | "===" -> S.Op2 (p, "stx=", sl, sr)
      | _ -> failwith ("fatal: unknown infix operator: " ^ op)
    end
  | E.IfExpr (p, e1, e2, e3) -> let e1 = ejs_to_ljs e1
    and e2 = ejs_to_ljs e2
    and e3 = ejs_to_ljs e3 in 
    S.If (p, 
      S.App (p, F.env_var p "%ToBoolean", [e1]),
      e2, 
      e3)
  | E.AssignExpr (p, obj, pr, vl) -> 
    let sobj = to_object p (ejs_to_ljs obj) in
    let spr = to_string p (ejs_to_ljs pr) in
    let svl = ejs_to_ljs vl in
    make_set_field p sobj spr svl
  | E.AppExpr (p, e, el) -> 
    let sl = List.map ejs_to_ljs el in
    let args_obj = make_args_obj p false sl in
    let obj_id = mk_id "obj" in
    let fun_id = mk_id "fun" in
    begin match e with
      | E.BracketExpr (_, E.IdExpr (_, "%context"), E.String (_, "eval")) ->
        S.App (p, F.env_var p "%maybeDirectEval", [S.Id (p, "%this"); S.Id (p, "%context"); args_obj; S.Id (p, "#strict")])
      | E.BracketExpr (_, E.IdExpr (_, "%context"), _) ->
        S.Let (p, fun_id, ejs_to_ljs e,
          appexpr_check (S.Id (p, fun_id))
          (S.App (p, S.Id (p, fun_id), [S.Undefined (p); args_obj]))
          p)
      | E.BracketExpr (_, obj, fld) ->
        let flde = ejs_to_ljs fld in
        S.Let (p, obj_id, ejs_to_ljs obj, 
          S.Let (p, fun_id, make_get_field p (to_object p (S.Id (p, obj_id))) (to_string p flde),
            appexpr_check (S.Id (p, fun_id))
            (S.App (p, S.Id (p, fun_id), [to_object p (S.Id (p, obj_id)); args_obj]))
            p))
      | E.FuncExpr _
      | _ ->
        S.Let (p, fun_id, ejs_to_ljs e,
          appexpr_check (S.Id (p, fun_id))
          (S.App (p, S.Id (p, fun_id), [S.Undefined (p); args_obj]))
          p)
    end
  | E.FuncExpr (p, args, body) -> get_fobj p args body (S.Id (p, "%context"))
  | E.LetExpr (p, nm, vl, body) ->
    let sv = ejs_to_ljs vl
    and sb = ejs_to_ljs body in
    let result_obj = match nm with
      | "%context" -> let orig_props = match sv with
        | S.Object (_, _, pl, _) -> pl
        | _ -> failwith "let bound %context to a non-object" in
        let c_attrs = { S.primval = None;
                        S.code = None;
                        S.proto = Some (S.Id (p, "%context"));
                        S.klass = "Object";
                        S.extensible = true; } in
        S.Object (p, c_attrs, orig_props, [Top])
      | _ -> sv in
    S.Let (p, nm, result_obj, sb)
  | E.SeqExpr (p, e1, e2) -> S.Seq (p, ejs_to_ljs e1, ejs_to_ljs e2)
  | E.WhileExpr (p, tst, bdy) ->
    let t = ejs_to_ljs tst in
    let (b, after) = match bdy with
      | E.LetExpr (p, "%%after", after, real_bdy) ->
        (ejs_to_ljs real_bdy, ejs_to_ljs after)
      | _ -> (ejs_to_ljs bdy, S.Undefined (p)) in
    get_while (Pos.synth p) t b after
  | E.LabelledExpr (p, lbl, exp) -> S.Label (p, lbl, ejs_to_ljs exp)
  | E.BreakExpr (p, id, e) -> S.Break (p, id, ejs_to_ljs e)
  | E.ForInExpr (p, nm, vl, bdy) ->
    get_forin p nm (ejs_to_ljs vl) (ejs_to_ljs bdy)
    (* JavaScript exceptions are desugared to throw values that look
       like {%js-exn: <js-exn-value>}, and we desugar catch blocks to
       check for this pattern and rethrow if it's not found.  This lets
       the interpreter throw arbitrary values to signal special
       exceptions in its own effective namespace. *)
  | E.ThrowExpr (p, e) -> S.Throw (p, S.Object (p, S.d_attrs,
      [("%js-exn", S.Data ({
          S.value = ejs_to_ljs e;
          S.writable = false;
        }, false, false))], [Top]))
  | E.TryCatchExpr (p, body, ident, catch) -> 
    let to_js_exn = S.App (p, S.Id (p, "%ToJSError"), [S.Id (p, ident)]) in
    let new_ctxt = 
      S.Object (p, { S.d_attrs with S.proto = Some (S.Id (p, "%parent")) },
                [(ident, 
                  S.Data ({
                    S.value = to_js_exn;
                    S.writable = true
                  }, 
                  false, false) );], [Top])
    in
    S.TryCatch (p, ejs_to_ljs body, 
      S.Lambda(p, [ident], 
        S.Let (p, "%parent", S.Id (p, "%context"),
          S.Let (p, "%context", new_ctxt, 
            ejs_to_ljs catch))))
  | E.FuncStmtExpr (p, nm, args, body) -> 
    let fobj = get_fobj p args body (S.Id (p, "%context")) in
    let f_id = mk_id "fobj" in
    let arcrd = { S.value = S.Id (p, f_id); S.writable = true; } in
    let aprop = S.Data (arcrd, true, true) in
    let aprops = [("0", aprop)] in
    let argsobj = S.Object (p, S.d_attrs, aprops, [Top]) in
    S.Let (p, f_id, fobj,
      S.SetField (p, S.Id (p, "%context"), S.String (p, nm, [Top]), S.Id (p, f_id), argsobj))
  | E.TryFinallyExpr (p, body, finally) -> 
    S.TryFinally (p, ejs_to_ljs body, ejs_to_ljs finally)
  | E.SwitchExpr (p, d, cl) ->

    let or' a b = 
      S.If (p, a, S.True (p, [Top]), b) in
    let and' a b =
      S.If (p, a, b, S.False (p, [Top])) in
    let disc_id = mk_id "disc" in
    let disc = S.Id (p, disc_id) in

    if List.exists (fun c -> match c with E.Default _ -> true | _ -> false) cl
    then

      let (a_clauses, default , b_clauses ) =
        let (a_clauses, rest) = take_while (function
          | E.Default _ -> false
          | _ -> true) cl in
        let (default, b_clauses) = match rest with
          | [] -> None, []
          | hd::tl -> Some hd, tl in
        (a_clauses, default, b_clauses) in
        
      let rec loop i case = function
        | [] -> S.Null (p)
        | [a] -> case i a
        | a :: rest -> S.Seq (p, case i a, loop (i+1) case rest) in

      let case_to_ljs = function 
        | E.Case (p, test, body) ->
          (ejs_to_ljs test, ejs_to_ljs body)
        | _ -> failwith "no default" in

      let default_to_ljs = function 
        | Some (E.Default (p, body)) -> ejs_to_ljs body
        | _ -> failwith "Fatal: default_to_ljs received non-default expr" in


      let fid = mk_id "found" in
      let step5 rest = 
        let step5iter caseCount (tst, bdy) =
          S.Seq (p,
            S.If (p, 
              (and' (S.Op2 (p, "stx=", S.Id (p, fid), S.False (p, [Top])))
                    (S.Op2 (p, "stx=", disc, tst))),
              S.SetBang (p, fid, S.True (p, [Top])),
              S.Null (p)),
            S.If (p, 
              S.Id (p, fid),
              bdy,
              S.Null (p))) in
        S.Let (p, fid, S.False (p, [Top]), 
          S.Seq (p, 
            loop 0 step5iter (map case_to_ljs a_clauses),
            rest)) in

      let fib = mk_id "foundInB" in
      let casecountid = mk_id "casecount" in
      let step6 rest = 
        S.Let (p, fib, S.False (p, [Top]), rest) in

      let step7 rest = 
        let step7iter caseCount (tst, bdy) =
          (* This is what we think browsers are doing
          S.If (p, 
            (and' (S.Op2 (p, "stx=", S.Id (p, fib, S.False (p)))
                  (S.Op2 (p, "stx=", disc, tst))),
            S.Seq (p,
              S.SetBang (p, fib, S.True (p)),
              S.Seq (p, 
                S.SetBang (p, casecountid, S.Num(p, float_of_int caseCount)),
                bdy)),
            S.Null (p)) in
          *)
          (* This is what the spec says *)
          S.If (p,
            S.Op2 (p, "stx=", S.Id (p, fib), S.False (p, [Top])),
            S.Seq (p,
              S.SetBang (p, casecountid, S.Num (p, float_of_int caseCount, [Top])),
              S.If (p, 
                S.Op2 (p, "stx=", disc, tst),
                S.Seq (p,
                  S.SetBang (p, fib, S.True (p, [Top])),
                  bdy),
                S.Null (p))),
            S.Null (p)) in
        S.Let (p, casecountid, S.Num (p, -1., [Top]),
          S.Seq (p, 
            S.If (p, 
              S.Id (p, fid), 
              S.Null (p),
              loop 0 step7iter (map case_to_ljs b_clauses)),
            rest)) in

      let step8 rest = 
        S.Seq (p, 
          S.If (p, 
            S.Id (p, fib), 
            S.Null (p),
            default_to_ljs default),
          rest) in

      let step9 =
        let step9iter caseCount (tst, bdy) =
          S.If (p,
            S.Op2 (p, "<", S.Id (p, casecountid), S.Num (p,
            float_of_int(caseCount), [Top])),
            bdy,
            S.Null (p)) in
        loop 0 step9iter (map case_to_ljs b_clauses) in
      (* TODO(joe): No break that leads to %before except in
         js_to_exprjs... this is stringly typed code *)
      S.Label (p, "%before",
        S.Let(p, disc_id, ejs_to_ljs d, step5 (step6 (step7 (step8 step9)))))

    else
      let fallthrough = mk_id "fallthrough" in
      let case = function
        | E.Case (p, test, body) ->
          let stest = ejs_to_ljs test
          and sbody = ejs_to_ljs body in
          S.If (p, 
            (or' (S.Op2 (p, "stx=", disc, stest)) (S.Id (p,fallthrough))),
            S.Seq (p, 
              S.SetBang (p, fallthrough, S.True (p, [Top])),
              sbody),
            S.Null (p))
        | _ -> failwith "desugaring error: found default case" in
      let rec cl_to_seq = function
        | [] -> S.Undefined (p)
        | [c] -> case c
        | c :: rest -> S.Seq (p, case c, cl_to_seq rest) in
      S.Label (p, "%before",
        S.Let (p, fallthrough, S.False (p, [Top]),
          S.Let (p, disc_id, ejs_to_ljs d,
            cl_to_seq cl)))
  | E.WithExpr (p, obj, body) ->
    S.Let (p, "%context", S.App (p, F.env_var p "%makeWithContext",
                                    [S.Id (p, "%context"); ejs_to_ljs obj]),
           ejs_to_ljs body)
  | E.StrictExpr (p, body) ->
    S.Let (p, "#strict", S.True (p, [Top]), ejs_to_ljs body)
  | E.NonstrictExpr (p, body) ->
    S.Let (p, "#strict", S.False (p, [Top]), ejs_to_ljs body)
  | E.HintExpr (p, s, e) ->
    S.Hint (p, s, ejs_to_ljs e)
  | E.AsExpr (p, e, ann) ->
    S.As (p, ejs_to_ljs e, parseAnn ann)
  | E.DropExpr (p, e, ann) ->
    S.Drop (p, ejs_to_ljs e, parseAnn ann)
  | E.CpAnnExpr (p, e1, e2) ->
    S.CpAnn (p, ejs_to_ljs e1, ejs_to_ljs e2)
  | E.AssertExpr (p, e1, ann) ->
    (*
      1. Create a lambda assertion using S.LambdaEnf
        - Takes a single argument enforced with ljs_ann.
        - All it does is return the argument.
      2. Pass this to an application of e1 to said LambdaEnf
    *)
    let ljs_e1 = ejs_to_ljs e1 in
    let ljs_ann = parseAnn ann in
    let argName = "SecurityAnnotationAssertionArg" in
    let body = S.Id(p, argName) in
    let assertFunc = S.LambdaEnf(p, [(argName, ljs_ann)], body) in
    S.App(p, assertFunc, [ljs_e1])
  | E.SecAnnExpr (p, ann) ->
    S.SecAnn (p, parseAnn ann)
  | E.ExtendsExpr (p, ann1, ann2) ->
    S.Extends (p, parseAnn ann1, parseAnn ann2)

and a_attrs pos = {
  S.primval = None;
      S.code = None;
      S.proto = Some (F.env_var pos "%ArrayProto");
      S.klass = "Array";
      S.extensible = true; }

and noargs_obj pos = S.Object (pos, a_attrs pos, [], [Top])

and onearg_obj pos a = 
  let r = { S.value = a; S.writable = true; } in
  let p = S.Data (r, true, true) in
  S.Object (pos, a_attrs pos, [("0", p)], [Top])

and get_fobj p args body context =
  let contains_illegals = 
    List.exists (fun nm -> (nm = "arguments") || (nm = "eval")) args in
  let uargs = remove_dupes args in
  if (uargs <> args) || contains_illegals then
    S.If (p, S.Id (p, "#strict"),
             S.App (p, F.env_var p "%SyntaxError", [S.String (p, "Illegal function definition", [Top])]),
             S.Undefined (p)) else
  let call = get_lambda p args body in
  let fproto = F.env_var p "%FunctionProto" in
  let fobj_attrs =
    { S.primval = None; S.code = Some (call); S.proto = Some (fproto); S.klass = "Function"; 
    S.extensible = true; } in
  let param_len = List.length args in
  let proto_id = mk_id "prototype" in
  let proto_obj = 
    S.Object (p, {S.d_attrs with S.proto = Some (F.env_var p "%ObjectProto")}, 
              [("constructor", S.Data ({ S.value = S.Undefined p;
                                         S.writable = true}, 
                                       false, false))], [Top]) in
  let proto_prop = S.Data ({ S.value = S.Id (p, proto_id); S.writable = true}, 
                           false, true) in
  let length_prop = S.Data ({ S.value = S.Num (p, (float_of_int param_len), [Top]); S.writable = true}, 
                           false, true) in
  let errorer = F.env_var p "%ThrowTypeError" in
  let errorer_prop = S.Accessor ({ S.getter = errorer; S.setter = errorer },
                                 false, false) in
  let func_id = mk_id "thisfunc" in
  S.Let (p, proto_id, proto_obj,
         S.Let (p, "%parent", context,
               S.Let (p, func_id, S.Object (p, fobj_attrs, [
                        ("prototype", proto_prop);
                        ("length", length_prop);
                        ("caller", errorer_prop);
                        ("arguments", errorer_prop)
                      ], [Top]),
                      S.Seq (p, S.SetField (p, S.Id (p, proto_id), S.String (p, "constructor", [Top]), S.Id (p, func_id), S.Null p),
                             S.Id (p, func_id)))))
           
and strip_lets e nms = match e with
  | E.StrictExpr (p, body) ->
    let (names, inner) = strip_lets body nms in
    (names, E.StrictExpr (p, inner))
  | E.NonstrictExpr (p, body) ->
    let (names, inner) = strip_lets body nms in
    (names, E.NonstrictExpr (p, inner))
  | E.LetExpr (p, nm, vl, rst) ->
    let prefix = if (String.length nm) >= 2 then String.sub nm 0 2 else "" in
    if prefix = "%%" then
      let l = (String.length nm) - 2 in
      let next_nms = (String.sub nm 2 l) :: nms in strip_lets rst next_nms
    else
      let (final_nms, final_e) = strip_lets rst nms in
      (final_nms, E.LetExpr (p, nm, vl, final_e))
  | _ -> (nms, e)

(* The first stage of desugaring creates exprjs let expressions corresponding
 * to JavaScript declared variables.  create_context is used to translate those
 * variables into the final representation (let-bound es5 variables with
 * randomly generated names, that are read/written by context object accessor
 * properties with the "real" name)
 *)
(* TODO(joe): overlapping vars and argument names goes here *)
and create_context p args body parent =
  let rec add_props props e =
    List.fold_right (fun prop e -> S.Let (p, prop, S.Undefined p, e)) props e
  and add_arg param index e = S.Let (p, param, S.GetField (p, S.Id (p, "%args"), S.String (p, string_of_int index, [Top]), S.Null p), e)
  and add_args args' e = List.fold_right2 add_arg args' (Prelude.iota (List.length args')) e
  and get_prop_pairs nms uids prs = 
    let getter uid = 
      S.Lambda (p, ["this"; "args"], 
        S.Label (p, "%ret",
          S.Break (p, "%ret", S.Id (p, uid))))
    and setter uid =
      let newval = S.GetField (p, S.Id (p, "args"), S.String (p, "0", [Top]), noargs_obj (Pos.synth p)) in
      (* TODO(joe): unused variable: what's it doing here? *)
      (* let setterao = onearg_obj newval in *)
      (* TODO(joe): %args vs args, this is perilous and confusing *)
      S.Lambda (p, ["this"; "args"],
      S.Label (p, "%ret",
      S.Break (p, "%ret",
      S.SetBang (p, uid, newval)))) in
    match nms, uids with
    | [], [] -> prs
    | nm :: rest, uid :: uid_rest ->
      let arc = { S.getter = getter uid; S.setter = setter uid; } in
      let a = S.Accessor (arc, false, false) in
      get_prop_pairs rest uid_rest ((nm, a) :: prs)
    | _ -> failwith "Fatal: unmatched id/arg lengths in create_context" in
  let c_attrs = { 
    S.primval = Some (S.False (p, [Top]));
    S.code = None; 
    S.proto = parent;
    S.klass = "Object";
    S.extensible = true; } in
  let (nl, real_body) = strip_lets body [] in
  let uids = List.map mk_id nl in
  let uids' = List.map mk_id args in
  let prop_pairs = get_prop_pairs (nl@args) (uids@uids') [] in
  (* TODO(joe): is data good enough here?  what about using arguments
   * in a catch block? *)
  let arg_prop = ("arguments", S.Data ({
    S.value = S.Id (p, "%args");
    S.writable = true;
  }, false, false)) in
  (uids, real_body, (add_props uids (add_args uids' (S.Object (p, c_attrs, arg_prop::prop_pairs, [Top])))))

and get_lambda p args body = 
  let (uids, real_body, ncontext) = create_context p args body (Some (S.Id (p, "%parent"))) in
  let desugared = ejs_to_ljs real_body in
  S.Lambda (p, ["%this"; "%args"],
    S.Seq (p,
      S.DeleteField (p, S.Id (p, "%args"), S.String (p, "%new", [Top])),
      S.Label (p, "%ret",
        S.Let (p, "%this", S.App (p, F.env_var p "%resolveThis", [S.Id (p, "#strict"); S.Id (p, "%this")]),
          S.Let (p, "%context", ncontext, S.Seq (p, desugared, S.Undefined (p)))))))

and remove_dupes lst =
  let rec helper l seen result = match l with
    | [] -> result
    | first :: rest ->
      let next = if (List.mem first seen) then result else (first :: result) in
      helper rest (first :: seen) next in
  List.rev (helper lst [] [])

and get_while p tst body after =
  (* This is to insert the label (if it exists) at 
   * the correct location in the desugared code *)
  let real_body = match body with
    | S.Label (_, nm, 
        S.Seq (_, init, 
          S.Label (_, 
            before, 
            S.Rec (_, 
              whilenm, 
              S.Lambda (_, 
                args, 
                S.Let (_, 
                  resultnm, 
                  testapp, 
                  S.If (_, 
                    result, 
                    S.Seq (_, 
                      S.Label (_, clbl, bdyapp),
                      e2),
                    e3))),
              whileapp)))) ->
      S.Seq (p, init, 
        S.Label (p, 
          before, 
          S.Rec (p, 
            whilenm, 
            S.Lambda (p, 
              args, 
              S.Let (p, 
                resultnm, 
                testapp, 
                S.If (p, 
                  result, 
                  S.Seq (p, 
                    S.Label (p, nm, bdyapp),
                    e2),
                  e3))),
            whileapp)))
    | _ -> body in
  let test = match tst with
    | S.Label (_, "%%dowhile", real_test) ->
      let ft_id = mk_id "firsttest" in
      S.Let (p, ft_id, S.True (p, [Top]),
        S.Lambda (p, [],
          S.If (p, S.Id (p, ft_id),
            S.Seq (p, S.SetBang (p, ft_id, S.False (p, [Top])), S.True (p, [Top])),
            real_test)))
    | _ -> S.Lambda (p, [], tst)
  and bdy = S.Lambda (p, [], real_body)
  and aftr = S.Lambda (p, [], after) in
  let tst, bdyid, afterid = mk_id "tst", mk_id "bdy", mk_id "after" in
  let whil, result = mk_id "while", mk_id "result" in
  let cont = "%continue" in
  S.Rec (p, whil,
    S.Lambda (p, [tst; bdyid; afterid],
      S.Let (p, result, 
        S.App (p, F.env_var p "%ToBoolean",
          [S.App (p, S.Id (p, tst), [])]),
        S.If (p, S.Id (p, result),
          S.Seq (p, 
            S.Label (p, cont, S.App (p, S.Id (p, bdyid), [])),
            S.Seq (p, 
              S.App (p, S.Id (p, afterid), []),
              S.App (p, S.Id (p, whil), 
                [S.Id (p, tst); S.Id (p, bdyid); S.Id (p, afterid)]))),
          S.Undefined (p)))),
    S.App (p, S.Id (p, whil), [test; bdy; aftr]))

and prop_itr p = 
  let tst =
    S.Op2 (p, "hasOwnProperty", 
      S.Id (p, "%obj"), 
      S.Op1 (p, "prim->str", S.Id (p, "%index")))
  and cns = 
    S.Let (p, "%rval",
    S.GetField (p, S.Id (p, "%obj"), S.Op1 (p, "prim->str", S.Id (p, "%index")),
    S.Null (p)),
    S.Seq (p,
    S.SetBang (p, "%index", S.Op2 (p, "+", S.Id (p ,"%index"), S.Num (p, 1., [Top]))),
    S.Id (p, "%rval"))) in
  S.Lambda (p, ["%obj"],
    S.Let (p, "%index", S.Num (p, 0., [Top]),
      S.Lambda (p, [],
        S.If (p, tst, cns, S.Undefined (p)))))

and get_forin p nm robj bdy = (* TODO: null args object below!! *)
  let context = S.Id (p, "%context")
  and nms = S.String (p, nm, [Top]) in
  let tst =
    S.Op1 (p, "!", undef_test p (S.GetField (p, context, nms, S.Null (p))))
  and after =
    make_set_field p context nms (S.App (p, S.Id (p, "%prop_itr"), [])) in
  let doloop_id = mk_id "do_loop" in
  S.Let (p, doloop_id,
    S.Lambda (p, [], 
      S.Rec (p, "%get_itr", prop_itr (Pos.synth p),
      S.Let (p, "%pnameobj", S.App (p, S.Id (p, "%propertyNames"), [robj; S.False (p, [Top])]),
      S.Let (p, "%prop_itr", S.App (p, S.Id (p, "%get_itr"), [S.Id (p, "%pnameobj")]),
      S.Seq (p, 
              S.App (p, 
                F.env_var p "%set-property",
                [context; nms; S.App (p, S.Id (p, "%prop_itr"), [])]),
             get_while (Pos.synth p) tst bdy after))))),
    S.If (p, undef_test p robj,
      S.Undefined (p),
      S.If (p, S.Op2 (p, "stx=", robj, S.Null (p)),
        S.Undefined (p),
        S.App (p, S.Id (p, doloop_id), []))))

and appexpr_check f app p = 
  let ftype = mk_id "ftype" in
  let not_function =
    S.Op1 (p, "!", S.Op2 (p, "stx=", S.Id (p, ftype), S.String (p, "function", [Top]))) in
  let error = throw_typ_error p "Not a function" in 
  S.Let (p, ftype, S.Op1 (p, "typeof", f),
    S.If (p, not_function, error, app))

let add_preamble p used_ids var_ids final = 
  let define_id id =
    S.App (p, F.env_var p "%defineGlobalAccessors", [S.Id (p, "%context"); S.String (p, id, [Top])]) in
  let define_var id =
    S.App (p, F.env_var p "%defineGlobalVar", [S.Id (p, "%context"); S.String (p, id, [Top])]) in
  let rec dops_of_ids def_fun lst base = match lst with
    | [] -> base
    | id :: rest -> S.Seq (p, def_fun id, dops_of_ids def_fun rest base) in
  dops_of_ids define_var var_ids (dops_of_ids define_id used_ids final)

let exprjs_to_ljs p (used_ids : IdSet.t) (e : E.expr) : S.exp =
  let (names, inner) = strip_lets e [] in
  let desugared = ejs_to_ljs inner in
  let is_strict = match inner with
    | E.StrictExpr _ -> true
    | E.NonstrictExpr _ -> false
    | _ -> failwith "exprjs_to_ljs: expected expression to be marked as strict or nonstrict" in
  let binder =
     F.env_var p (if is_strict then "%strictContext" else "%nonstrictContext") in
  S.Let (p, "%context", binder,
    add_preamble p (IdSet.elements used_ids) names desugared)

