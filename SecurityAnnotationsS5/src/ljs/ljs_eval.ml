open Prelude
module S = Ljs_syntax
open Format
open Ljs
open Ljs_values
open Ljs_delta
open Ljs_pretty
open Ljs_pretty_value
open Ljs_annotation
open Unix
open SpiderMonkey
open Exprjs_to_ljs
open Js_to_exprjs
open Str


type answer = Answer of S.exp list * value * env list * store

let progAnns = ref [Top] 

let bool b = match b with
  | true -> True [Top]
  | false -> False [Top]

let unbool b = match b with
  | True _ -> true
  | False _ -> false
  | _ -> failwith ("tried to unbool a non-bool" ^ (pretty_value b))

(*Gets the annotation on a value*)
let get_ann_val b store = match b with
  | True ann -> ann
  | False ann -> ann
  | String (_, ann) -> ann
  | Num (_, ann) -> ann
  | _ -> raise (AnnErr ([], String ("NotAnnotatable", [Top]), store)) 

(*Changes the annotation on a boolean*)
let set_ann_val b ann store = match b with
  | True _ -> True ann
  | False _ -> False ann
  | String (s, _) -> String (s, ann)
  | Num (n, _) -> Num (n, ann)
  | _ -> raise (AnnErr ([], String ("NotAnnotatable", [Top]), store)) 

(*Used to get the annotation store from some functions*)
let get_3 (_, _, t) = t

let update_global_anns ann store = 
  set_global_ann ann store

let enforce_annotation guard arg store =
  if (unbool (is_annotatable store arg)) then
    let argument_ann = match arg with
      | ObjLoc loc ->
         get_3 (get_obj store loc)
      | _ -> 
          get_ann_val arg store in
    compare_annotation argument_ann guard store
  else
    match guard with 
      | [Top] -> true
      | _ ->
         raise (AnnErr ([], String ("FailedAnnotationCheck", [Top]), store))

let interp_error pos message =
  raise (PrimErr ([], String (("[interp] (" ^ Pos.string_of_pos pos ^ ") " ^ message), [Top])))

let rec get_prop p store obj field =
  match obj with
    | Null -> None
    | ObjLoc loc -> begin match get_obj store loc with
      | { proto = pvalue; }, props, _ ->
         try Some (IdMap.find field props)
         with Not_found -> get_prop p store pvalue field
      end
    | _ -> failwith (interp_error p 
           "get_prop on a non-object.  The expression was (get-prop " 
         ^ pretty_value obj 
         ^ " " ^ field ^ ")")

let get_obj_attr attrs attr = match attrs, attr with
  | { proto=proto }, S.Proto -> proto
  | { extensible=extensible} , S.Extensible -> bool extensible
  | { code=Some code}, S.Code -> code
  | { code=None}, S.Code -> Null
  | { primval=Some primval}, S.Primval -> primval
  | { primval=None}, S.Primval ->
      failwith "[interp] Got Primval attr of None"
  | { klass=klass }, S.Klass -> String (klass, [Top])


let rec get_attr store attr obj field = match obj, field with
  | ObjLoc loc, String (s, _) -> let (attrs, props, _) = get_obj store loc in
      if (not (IdMap.mem s props)) then
        undef
      else
        begin match (IdMap.find s props), attr with 
          | Data (_, _, config), S.Config
          | Accessor (_, _, config), S.Config -> bool config
          | Data (_, enum, _), S.Enum
          | Accessor (_, enum, _), S.Enum -> bool enum
          | Data ({ writable = b; }, _, _), S.Writable -> bool b
          | Data ({ value = v; }, _, _), S.Value -> v
          | Accessor ({ getter = gv; }, _, _), S.Getter -> gv
          | Accessor ({ setter = sv; }, _, _), S.Setter -> sv
          | _ -> interp_error Pos.dummy "bad access of attribute"
        end
  | _ -> failwith ("[interp] get-attr didn't get an object and a string.")

(* 
   The goal here is to maintain a few invariants (implied by 8.12.9
   and 8.10.5), while keeping things simple from a semantic
   standpoint.  The errors from 8.12.9 and 8.10.5 can be defined in
   the environment and enforced that way.  The invariants here make it
   more obvious that the semantics can't go wrong.  In particular, a
   property

   1.  Has to be either an accessor or a data property, and;

   2.  Can't change attributes when Config is false, except for 
       a. Value, which checks Writable
       b. Writable, which can change true->false
*)
let rec set_attr (store : store) attr obj field newval = match obj, field with
  | ObjLoc loc, String (f_str, _) -> begin match get_obj store loc with
      | ({ extensible = ext; } as attrsv, props, ann) ->
        if not (IdMap.mem f_str props) then
          if ext then 
            let newprop = match attr with
              | S.Getter -> 
                Accessor ({ getter = newval; setter = Undefined; }, 
                          false, false)
              | S.Setter -> 
                Accessor ({ getter = Undefined; setter = newval; }, 
                          false, false)
              | S.Value -> 
                Data ({ value = newval; writable = false; }, false, false)
              | S.Writable ->
                Data ({ value = Undefined; writable = unbool newval },
                      false, false) 
              | S.Enum ->
                Data ({ value = Undefined; writable = false },
                      unbool newval, true) 
              | S.Config ->
                Data ({ value = Undefined; writable = false },
                      true, unbool newval) in
            let store = set_obj store loc
                  (attrsv, IdMap.add f_str newprop props, [Top]) in
            true, store
          else
            failwith "[interp] Extending inextensible object ."
        else
        (* 8.12.9: "If a field is absent, then its value is considered
        to be false" -- we ensure that fields are present and
        (and false, if they would have been absent). *)
          let newprop = match (IdMap.find f_str props), attr, newval with
            (* S.Writable true -> false when configurable is false *)
            | Data ({ writable = true } as d, enum, config), S.Writable, new_w -> 
              Data ({ d with writable = unbool new_w }, enum, config)
            | Data (d, enum, true), S.Writable, new_w ->
              Data ({ d with writable = unbool new_w }, enum, true)
            (* Updating values only checks writable *)
            | Data ({ writable = true } as d, enum, config), S.Value, v ->
              Data ({ d with value = v }, enum, config)
            (* If we had a data property, update it to an accessor *)
            | Data (d, enum, true), S.Setter, setterv ->
              Accessor ({ getter = Undefined; setter = setterv }, enum, true)
            | Data (d, enum, true), S.Getter, getterv ->
              Accessor ({ getter = getterv; setter = Undefined }, enum, true)
            (* Accessors can update their getter and setter properties *)
            | Accessor (a, enum, true), S.Getter, getterv ->
              Accessor ({ a with getter = getterv }, enum, true)
            | Accessor (a, enum, true), S.Setter, setterv ->
              Accessor ({ a with setter = setterv }, enum, true)
            (* An accessor can be changed into a data property *)
            | Accessor (a, enum, true), S.Value, v ->
              Data ({ value = v; writable = false; }, enum, true)
            | Accessor (a, enum, true), S.Writable, w ->
              Data ({ value = Undefined; writable = unbool w; }, enum, true)
            (* enumerable and configurable need configurable=true *)
            | Data (d, _, true), S.Enum, new_enum ->
              Data (d, unbool new_enum, true)
            | Data (d, enum, true), S.Config, new_config ->
              Data (d, enum, unbool new_config)
            | Data (d, enum, false), S.Config, False _ -> 
              Data (d, enum, false)
            | Accessor (a, enum, true), S.Config, new_config ->
              Accessor (a, enum, unbool new_config)
            | Accessor (a, enum, true), S.Enum, new_enum ->
              Accessor (a, unbool new_enum, true)
            | Accessor (a, enum, false), S.Config, False _ ->
              Accessor (a, enum, false)
            | _ -> raise (PrimErr ([], String(("[interp] bad property set "
                    ^ (pretty_value obj) ^ " " ^ f_str ^ " " ^
                    (S.string_of_attr attr) ^ " " ^ (pretty_value newval), [Top]))))
        in begin
            let store = set_obj store loc 
              (attrsv, IdMap.add f_str newprop props, [Top]) in
            true, store
        end
  end
  | _ ->
    let msg = (sprintf "[interp] set-attr got: %s[%s] not object and string"
                  (pretty_value obj) (pretty_value field)) in
    raise (PrimErr ([], String(msg, [Top])))

let rec eval desugar exp env (store : store) : (value * store) =
  let eval exp env store =
    begin try eval desugar exp env store
      with 
      | Break (exprs, l, v, s) ->
        raise (Break (exp::exprs, l, v, s))
      | Throw (exprs, v, s) ->
        raise (Throw (exp::exprs, v, s))
      | AnnErr (exprs, v, s) ->
        raise (AnnErr (exp::exprs, v, s))
      | PrimErr (exprs, v) ->
        raise (PrimErr (exp::exprs, v))
      | Snapshot (exps, v, envs, s) ->
        raise (Snapshot (exp :: exps, v, env :: envs, s))
      | Sys.Break -> raise
          (PrimErr ([exp], String("s5_eval stopped by user interrupt", [Top])))
      | Stack_overflow -> raise
          (PrimErr ([exp], String("s5_eval overflowed the Ocaml stack", [Top])))
    end in
  let rec apply p store func args = match func with
    | Closure (env, xs, body) ->
      let alloc_arg argval argname (store, env) =
        let (new_loc, store) = add_var store argval in
        let env' = IdMap.add argname new_loc env in
        (store, env') in
      if (List.length args) != (List.length xs) then
        arity_mismatch_err p xs args
      else
        let (store, env) = (List.fold_right2 alloc_arg args xs (store, env)) in
        eval body env store
    | ClosureEnf (env, x_ann_s, body) ->
      let xs = List.map (function (x,y) -> x) x_ann_s in
      if (List.length args) != (List.length xs) then
        arity_mismatch_err p xs args
      else
        let enforceable_anns = List.map (function (x,y) -> y) x_ann_s in
        let pred x y = enforce_annotation x y store in
        if (List.for_all2 pred enforceable_anns args) then
          let passed = Closure (env, xs, body) in
          apply p store passed args
        else
          raise (AnnErr ([], String ("FailedAnnotationCheck", [Top]), store))
          (*failwith "FailedAnnotationCheck"*)
    | ObjLoc loc -> begin match get_obj store loc with
        | ({ code = Some f }, _, _) -> apply p store f args
        | _ -> failwith "Applied an object without a code attribute"
      end
    | _ -> failwith (interp_error p
                       ("Applied non-function, was actually " ^ 
                         pretty_value func)) in
  match exp with
  | S.Hint (_, "___takeS5Snapshot", e) ->
    let (v, store) = eval e env store in
    raise (Snapshot ([], v, [], store))
  | S.Hint (_, _, e) -> eval e env store
  | S.SecAnn (_, annotation) -> 
      let store = (update_global_anns annotation store) in
      Undefined, store
  | S.Extends (_, sub, super) ->
      (*
       * Error Handling: as per paper, super must be atomic
       *)
      if (List.length super == 1) then
        (*
         * If super is not currently described as having subs in the hierarchy,
         * add it with empty sub annotations.
         * We then pull the super into its atomic form for use as a key in the
         * map encoding the hierarchy.
         *)
        let store = update_global_anns super store in
        let super = List.hd super in
        (*
         * 2. Check sub is not a valid annotation.
         *)
        if (not (check_ann_valid sub store)) then
          (*
           * 3. Add sub to the list of subannotations of super.
           * 4. Evaluate to undefined.
           *)
           let store = update_sub_of_super super sub store in
           Undefined, store
        else 
          raise (AnnErr ([], String ("SubAnnotationAlreadyExists", [Top]), store))
      else
        raise (AnnErr ([], String ("SuperAnnotationNotAtomic", [Top]), store))
  | S.Undefined _ -> Undefined, store
  | S.Null _ -> Null, store
  | S.String (_, s, annotation) ->
      if (check_ann_valid annotation store)
        then String (s, (remove_dup_ann annotation)), store
        else raise (AnnErr ([], String ("UndeclaredAnnotation", [Top]), store))
  | S.Num (_, n, annotation) ->
      if (check_ann_valid annotation store)
        then Num (n, (remove_dup_ann annotation)), store
        else raise (AnnErr ([], String ("UndeclaredAnnotation", [Top]), store))
  | S.True (_, annotation) -> 
      if (check_ann_valid annotation store)
        then True (remove_dup_ann annotation), store
        else raise (AnnErr ([], String ("UndeclaredAnnotation", [Top]), store))
  | S.False (_, annotation) ->
      if (check_ann_valid annotation store) 
        then False (remove_dup_ann annotation), store
        else raise (AnnErr ([], String ("UndeclaredAnnotation", [Top]), store))
  | S.Id (p, x) -> begin
      try
        (get_var store (IdMap.find x env), store)
      with Not_found ->
        failwith ("[interp] Unbound identifier: " ^ x ^ " in identifier lookup at " ^
                    (Pos.string_of_pos p))
    end
  | S.SetBang (p, x, e) -> begin
      try
        let loc = IdMap.find x env in
        let (new_val, store) = eval e env store in
        let store = set_var store loc new_val in
        new_val, store
      with Not_found ->
        failwith ("[interp] Unbound identifier: " ^ x ^ " in set! at " ^
                    (Pos.string_of_pos p))
    end
  | S.Object (p, attrs, props, annotation) -> 
    if (not (check_ann_valid annotation store))
      then raise (AnnErr ([], String ("UndeclaredAnnotation", [Top]), store))
      else
        let { S.primval = vexp;
            S.proto = protoexp;
            S.code = codexp;
            S.extensible = ext;
            S.klass = kls; } = attrs in
      let opt_lift (value, store) = (Some value, store) in
      let primval, store = match vexp with
        | Some vexp -> opt_lift (eval vexp env store)
        | None -> None, store
      in
      let proto, store = match protoexp with 
        | Some pexp -> eval pexp env store
        | None -> Undefined, store
      in
      let code, store = match codexp with
        | Some cexp -> opt_lift (eval cexp env store)
        | None -> None, store
      in
      let attrsv = {
        code=code; proto=proto; primval=primval;
        klass=kls; extensible=ext;
      } in
      let eval_prop prop store = match prop with
        | S.Data ({ S.value = vexp; S.writable = w; }, enum, config) ->
          let vexp, store = eval vexp env store in
          Data ({ value = vexp; writable = w; }, enum, config), store
        | S.Accessor ({ S.getter = ge; S.setter = se; }, enum, config) ->
          let gv, store = eval ge env store in
          let sv, store = eval se env store in
          Accessor ({ getter = gv; setter = sv}, enum, config), store
      in
      let eval_prop (m, store) (name, prop) = 
          let propv, store = eval_prop prop store in
            IdMap.add name propv m, store in
        let propsv, store =
          fold_left eval_prop (IdMap.empty, store) props in
        let obj_loc, store = add_obj store (attrsv, propsv, remove_dup_ann annotation) in
        ObjLoc obj_loc, store
    (* 8.12.4, 8.12.5 *)
  | S.SetField (p, obj, f, v, args) ->
      let (obj_value, store) = eval obj env store in
      let (f_value, store) = eval f env store in
      let (v_value, store) = eval v env store in
      let (args_value, store) = eval args env store in begin
        match (obj_value, f_value) with
          | (ObjLoc loc, String (s, _)) ->
            let ({extensible=extensible;} as attrs, props, ann) =
              get_obj store loc in
            let prop = get_prop p store obj_value s in
            let unwritable = (Throw ([],
              String ("unwritable-field", [Top]),
              store
            )) in
            begin match prop with
              | Some (Data ({ writable = true; }, enum, config)) ->
                let (enum, config) = 
                  if (IdMap.mem s props)
                  then (enum, config) (* 8.12.5, step 3, changing the value of a field *)
                  else (true, true) in (* 8.12.4, last step where inherited.[[writable]] is true *)
                let store = set_obj store loc 
                    (attrs,
                      IdMap.add s
                        (Data ({ value = v_value; writable = true },
                               enum, config))
                        props, [Top]) in
                v_value, store
              | Some (Data _) -> raise unwritable
              | Some (Accessor ({ setter = Undefined; }, _, _)) ->
                raise unwritable
              | Some (Accessor ({ setter = setterv; }, _, _)) ->
                (* 8.12.5, step 5 *)
                apply p store setterv [obj_value; args_value]
              | None ->
                (* 8.12.5, step 6 *)
                if extensible
                then
                  let store = set_obj store loc 
                      (attrs,
                        IdMap.add s 
                          (Data ({ value = v_value; writable = true; },
                                 true, true))
                          props, [Top]) in
                  v_value, store
                else
                  Undefined, store (* TODO: Check error in case of non-extensible *)
            end
          | _ -> failwith ("[interp] Update field didn't get an object and a string" 
                           ^ Pos.string_of_pos p ^ " : " ^ (pretty_value obj_value) ^ 
                             ", " ^ (pretty_value f_value))
      end
  | S.GetField (p, obj, f, args) ->
      let (obj_value, store) = eval obj env store in
      let (f_value, store) = eval f env store in 
      let (args_value, store) = eval args env store in begin
        match (obj_value, f_value) with
          | (ObjLoc _, String (s, _)) ->
            let prop = get_prop p store obj_value s in
            begin match prop with
              | Some (Data ({value=v;}, _, _)) -> v, store
              | Some (Accessor ({getter=g;},_,_)) ->
                if g = Undefined
                then Undefined, store
                else apply p store g [obj_value; args_value]
              | None -> Undefined, store
            end
          | _ -> failwith ("[interp] Get field didn't get an object and a string at " 
                 ^ Pos.string_of_pos p 
                 ^ ". Instead, it got " 
                 ^ pretty_value obj_value 
                 ^ " and " 
                 ^ pretty_value f_value)
      end
  | S.DeleteField (p, obj, f) ->
      let (obj_val, store) = eval obj env store in
      let (f_val, store) = eval f env store in begin
        match (obj_val, f_val) with
          | (ObjLoc loc, String (s, _)) -> 
            begin match get_obj store loc with
              | (attrs, props, _) -> begin try
                match IdMap.find s props with
                  | Data (_, _, true) 
                  | Accessor (_, _, true) ->
                    let store = set_obj store loc
                      (attrs, IdMap.remove s props, [Top]) in
                    True [Top], store
                  | _ ->
                    raise (Throw ([], String("unconfigurable-delete", [Top]), store))
                with Not_found -> False [Top], store
              end
            end
          | _ -> failwith ("[interp] Delete field didn't get an object and a string at " 
                           ^ Pos.string_of_pos p 
                           ^ ". Instead, it got " 
                           ^ pretty_value obj_val
                           ^ " and " 
                           ^ pretty_value f_val)
        end
  | S.GetAttr (p, attr, obj, field) ->
      let (obj_val, store) = eval obj env store in
      let (f_val, store) = eval field env store in
        get_attr store attr obj_val f_val, store
  | S.SetAttr (p, attr, obj, field, newval) ->
      let (obj_val, store) = eval obj env store in
      let (f_val, store) = eval field env store in
      let (v_val, store) = eval newval env store in
      let b, store = set_attr store attr obj_val f_val v_val in
      bool b, store
  | S.GetObjAttr (p, oattr, obj) ->
      let (obj_val, store) = eval obj env store in
      begin match obj_val with
        | ObjLoc loc ->
            let (attrs, _, _) = get_obj store loc in
            get_obj_attr attrs oattr, store
        | _ -> failwith ("[interp] GetObjAttr got a non-object: " ^
                          (pretty_value obj_val))
      end
  | S.SetObjAttr (p, oattr, obj, attre) ->
      let (obj_val, store) = eval obj env store in
      let (attrv, store) = eval attre env store in
      begin match obj_val with
        | ObjLoc loc ->
            let (attrs, props, _) = get_obj store loc in
            let attrs' = match oattr, attrv with
              | S.Proto, ObjLoc _
              | S.Proto, Null -> { attrs with proto=attrv }
              | S.Proto, _ ->
                  failwith ("[interp] Update proto failed: " ^
                            (pretty_value attrv))
              | S.Extensible, True _ -> { attrs with extensible=true }
              | S.Extensible, False _ -> { attrs with extensible=false }
              | S.Extensible, _ ->
                  failwith ("[interp] Update extensible failed: " ^
                            (pretty_value attrv))
              | S.Code, _ -> failwith "[interp] Can't update Code"
              | S.Primval, v -> { attrs with primval=Some v }
              | S.Klass, _ -> failwith "[interp] Can't update Klass" in
            attrv, set_obj store loc (attrs', props, [Top])
        | _ -> failwith ("[interp] SetObjAttr got a non-object: " ^
                          (pretty_value obj_val))
      end
  | S.OwnFieldNames (p, obj) ->
      let (obj_val, store) = eval obj env store in
      begin match obj_val with
        | ObjLoc loc ->
          let _, props, _ = get_obj store loc in
          let add_name n x m = 
            IdMap.add (string_of_int x) 
              (Data ({ value = String (n, [Top]); writable = false; }, false, false)) 
              m in
          let namelist = IdMap.fold (fun k v l -> (k :: l)) props [] in
          let props = 
            List.fold_right2 add_name namelist
              (iota (List.length namelist)) IdMap.empty
          in
          let d = (float_of_int (List.length namelist)) in
          let final_props = 
            IdMap.add "length" 
              (Data ({ value = Num (d, [Top]); writable = false; }, false, false))
              props
          in
          let (new_obj, store) = add_obj store (d_attrsv, final_props, [Top]) in
          ObjLoc new_obj, store
        | _ -> failwith ("[interp] OwnFieldNames didn't get an object," ^
                  " got " ^ (pretty_value obj_val) ^ " instead.")
      end
  | S.Op1 (p, op, e) ->
      let (e_val, store) = eval e env store in
      op1 store op e_val, store
  | S.Op2 (p, op, e1, e2) -> 
      let (e1_val, store) = eval e1 env store in
      let (e2_val, store) = eval e2 env store in
      op2 store op e1_val e2_val, store
  | S.If (p, c, t, e) ->
      let (c_val, store) = eval c env store in
        if (c_val = True [Top]) (*TODO (DUNCAN): THIS SEEMS INCORRECT*)
        then eval t env store
        else eval e env store
  | S.App (p, func, args) -> 
      let (func_value, store) = eval func env store in
      let (args_values, store) =
        fold_left (fun (vals, store) e ->
            let (newval, store) = eval e env store in
            (newval::vals, store))
          ([], store) args in
      apply p store func_value (List.rev args_values)
  | S.Seq (p, e1, e2) -> 
      let (_, store) = eval e1 env store in
      eval e2 env store
  | S.Let (p, x, e, body) ->
      let (e_val, store) = eval e env store in
      let (new_loc, store) = add_var store e_val in
      eval body (IdMap.add x new_loc env) store
  | S.Rec (p, x, e, body) ->
      let (new_loc, store) = add_var store Undefined in
      let env' = (IdMap.add x new_loc env) in
      let (ev_val, store) = eval e env' store in
      eval body env' (set_var store new_loc ev_val)
  | S.Label (p, l, e) ->
      begin
        try
          eval e env store
        with Break (t, l', v, store) ->
          if l = l' then (v, store)
          else raise (Break (t, l', v, store))
      end
  | S.Break (p, l, e) ->
      let v, store = eval e env store in
      raise (Break ([], l, v, store))
  | S.TryCatch (p, body, catch) -> begin
      try
        eval body env store
      with
        | Throw (_, v, store) ->
          let catchv, store = eval catch env store in
            apply p store catchv [v]
        | AnnErr (_, v, store) ->
          let catchv, store = eval catch env store in
            apply p store catchv [v]
    end
  | S.TryFinally (p, body, fin) -> begin
      try
        let (v, store) = eval body env store in
        let (_, store') = eval fin env store in
        (v, store')
      with
        | Throw (t, v, store) ->
          let (_, store) = eval fin env store in
          raise (Throw (t, v, store))
        | AnnErr (t, v, store) ->
          let (_, store) = eval fin env store in
          raise (AnnErr (t, v, store))
        | Break (t, l, v, store) ->
          let (_, store) = eval fin env store in
          raise (Break (t, l, v, store))
      end
  | S.Throw (p, e) -> let (v, s) = eval e env store in
    raise (Throw ([], v, s))
  | S.Lambda (p, xs, e) ->
    (* Only close over the variables that the function body might reference. *)
    let free_vars = S.free_vars e in
    let filtered_env =
      IdMap.filter (fun var _ -> IdSet.mem var free_vars) env in
    Closure (filtered_env, xs, e), store
  | S.LambdaEnf (p, xs, e) ->
    (* Only close over the variables that the function body might reference. *)
    let free_vars = S.free_vars e in
    let filtered_env =
      IdMap.filter (fun var _ -> IdSet.mem var free_vars) env in
    ClosureEnf (filtered_env, xs, e), store
    (* Annotation Adding and removal
     * 
     * check annotatable enforces non-null, non-undefined, non-object.
     *
     *)
  | S.As (p, e, ann) ->
    let (e_val, store) = eval e env store in
    let check_annotatable = unbool (is_annotatable store e_val) in
    if (check_annotatable) 
    then 
      if (check_ann_valid ann store)
      then match e_val with
        | ObjLoc loc ->
          let (attrs, props, old_ann) = get_obj store loc in
          let new_ann = remove_dup_ann (add_ann old_ann ann) in
          let store = set_obj store loc (attrs, props, new_ann) in
          e_val, store
        | _ -> 
          let old_ann = get_ann_val e_val store in
          let new_ann = remove_dup_ann (add_ann old_ann ann) in
          let new_val = set_ann_val e_val new_ann store in
          new_val, store
      else raise (AnnErr ([], String ("UndeclaredAnnotation", [Top]), store))
    else raise (AnnErr ([], String ("NotAnnotatable", [Top]), store))
  | S.Drop (p, e, ann) ->
    let (e_val, store) = eval e env store in
    let check_annotatable = unbool (is_annotatable store e_val) in
    if (check_annotatable) 
    then 
      if (check_ann_valid ann store)
      then match e_val with
        | ObjLoc loc ->
          let (attrs, props, old_ann) = get_obj store loc in
          let new_ann = remove_dup_ann (cut old_ann ann store) in
          let store = set_obj store loc (attrs, props, new_ann) in
          e_val, store
        | _ -> 
          let old_ann = get_ann_val e_val store in
          let new_ann = remove_dup_ann (cut old_ann ann store) in
          let new_val = set_ann_val e_val new_ann store in
          new_val, store
      else raise (AnnErr ([], String ("UndeclaredAnnotation", [Top]), store))
    else raise (AnnErr ([], String ("NotAnnotatable", [Top]), store))
  | S.CpAnn (pos, e1, e2) ->
    let (e1_val, store) = eval e1 env store in
    let (e2_val, store) = eval e2 env store in
    let check_annotatable = 
      unbool (is_annotatable store e1_val) && unbool (is_annotatable store e2_val) in
    if (check_annotatable)
      then
        let copied_ann = match e1_val with
          | ObjLoc loc1 -> get_3 (get_obj store loc1)
          | _ -> get_ann_val e1_val store in
        match e2_val with
          | ObjLoc loc2 ->
            let (attrs, props, old_ann) = get_obj store loc2 in
            let new_ann = remove_dup_ann (add_ann old_ann copied_ann) in
            let store = set_obj store loc2 (attrs, props, new_ann) in
            e2_val, store
          | _ ->
            let old_ann = get_ann_val e2_val store in
            let new_ann = remove_dup_ann (add_ann old_ann copied_ann) in
            let new_val = set_ann_val e2_val new_ann store in
            new_val, store
      else raise (AnnErr ([], String ("NotAnnotatable", [Top]), store))
  | S.Eval (p, e, bindings) ->
    let evalstr, store = eval e env store in
    let bindobj, store = eval bindings env store in
    begin match evalstr, bindobj with
      | String (s, _), ObjLoc o ->
        let expr = desugar s in
        let env, store = envstore_of_obj p (get_obj store o) store in
        eval expr env store
      | String (s, _), _ -> interp_error p "Non-object given to eval() for env"
      | v, _ -> v, store
    end

and envstore_of_obj p (_, props, _) store =
  IdMap.fold (fun id prop (env, store) -> match prop with
    | Data ({value=v}, _, _) ->
      let new_loc, store = add_var store v in
      let env = IdMap.add id new_loc env in
      env, store
    | _ -> interp_error p "Non-data value in env_of_obj")
  props (IdMap.empty, store)

and arity_mismatch_err p xs args = interp_error p ("Arity mismatch, supplied " ^ string_of_int (List.length args) ^ " arguments and expected " ^ string_of_int (List.length xs) ^ ". Arg names were: " ^ (List.fold_right (^) (map (fun s -> " " ^ s ^ " ") xs) "") ^ ". Values were: " ^ (List.fold_right (^) (map (fun v -> " " ^ pretty_value v ^ " ") args) ""))

let err show_stack trace message ann_err = 
  if show_stack then begin
      eprintf "%s\n" (string_stack_trace trace);
      eprintf "%s\n" message;
      failwith (if ann_err then "Annotation Error" else "Runtime Error")
    end
  else
    eprintf "%s\n" message;
    failwith (if ann_err then "Annotation Error" else "Runtime Error")


let continue_eval expr desugar print_trace env store = try
  Sys.catch_break true;
  let (v, store) = eval desugar expr env store in
  Answer ([], v, [], store)
with
  | Snapshot (exprs, v, envs, store) ->
    Answer (exprs, v, envs, store)
  | Throw (t, v, store) ->
      let err_msg = 
        match v with
          | ObjLoc loc -> begin match get_obj store loc with
            | _, props, _ -> try match IdMap.find "%js-exn" props with
              | Data ({value=jserr}, _, _) -> 
                string_of_value jserr store
              | _ -> string_of_value v store
              with Not_found -> string_of_value v store
            end
          | v -> string_of_value v store in
        if (err_msg = "\"FailedAnnotationCheck\"<!Top!>")
        then
          err print_trace t (sprintf "Annotation Error: %s\n" err_msg) true
        else
          err print_trace t (sprintf "Uncaught exception: %s\n" err_msg) false
  | Break (p, l, v, _) -> failwith ("Broke to top of execution, missed label: " ^ l)
  | PrimErr (t, v) ->
      err print_trace t (sprintf "Uncaught error: %s\n" (pretty_value v)) false
  | AnnErr (t, v, store) -> 
      let err_msg = string_of_value v store in
      err print_trace t (sprintf "Annotation Error: %s\n" err_msg) true

let eval_expr expr desugar print_trace =
  continue_eval expr desugar print_trace IdMap.empty (Store.empty, Store.empty, Hierarchy.empty)
