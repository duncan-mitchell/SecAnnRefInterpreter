open Prelude
open Unix
module S = Ljs_syntax
open Format
open Ljs
open Ljs_sym_values
open Ljs_sym_env
open Ljs_sym_pretty
open Ljs_sym_delta
open Ljs_sym_z3
open Ljs_sym_gc
open Ljs_pretty
open SpiderMonkey
open Exprjs_to_ljs
open Js_to_exprjs
open Str

(* How many times we should recursively check sym prototypes *)
let max_sym_proto_depth = 0

(* Create a new sym val *)
let new_sym_keyword = "NEWSYM"
(* Create a new sym val that can't be a pointer to a concrete object.
 * This is still sound, since it will still be able to be a pointer
 * to a sym object, which subsumes all concrete objects. *)
let fresh_sym_keyword = "NEWSYM_FRESH"

(* Hints *)
(* Objects created after this will be included in the set of objects that a new
 * symbolic value might point to. *)
let start_sym_keyword = "START SYM EVAL"
(* Objects created after this will NOT be included ... etc. *)
let stop_sym_keyword = "STOP SYM EVAL"
(* Summarize the given function (looked up by name). *)
let summarize_regex = Str.regexp "SUMMARIZE \\(.+\\)"

(* Flags *)
let print_store = false (* print store at each eval call *)
let summarize_functions = true (* compute and reuse symbolic summaries *)
let gc_on = true (* do garbage collection *) 

let do_gc envs pc = 
  if not gc_on then pc else
  { pc with store = garbage_collect envs pc.store }

let val_sym v = match v with SymScalar x -> (SId x) | _ -> (Concrete v)

let interp_error pos message =
  "[interp] (" ^ Pos.string_of_pos pos ^ ") " ^ message

let throw_str s = throw (Throw (String s))
  
let is_null_sym id = is_equal (SId id) (Concrete Null)

(* let string_to_num = *)
(*   let cache = IdHashtbl.create 10 in *)
(*   let count = ref 0 in *)
(*   (fun s -> *)
(*     try IdHashtbl.find s *)
(*     with Not_found -> *)
(*       incr count; IdHashtbl.add s !count; *)
(*       !count) *)
    
let arity_mismatch_err p xs args pc =
  failwith ("Arity mismatch, supplied " ^ string_of_int (List.length args) ^ 
               " arguments and expected " ^ string_of_int (List.length xs) ^ 
               " at " ^ Pos.string_of_pos p ^ ". Arg names were: " ^ 
               (List.fold_right (^) (map (fun s -> " " ^ s ^ " ") xs) "") ^ 
               ". Values were: " ^ 
               (List.fold_right (^) (map (fun v -> " " ^ 
                 val_to_string v ^ " ") args) ""))

let rec apply p func args envs pc nested_eval = match func with
  | Closure (arg_ids, body_exp, closure_env) ->
    let bind_arg arg id (envs, pc) = 
      let (loc, pc') = sto_alloc_val arg pc in 
      (Env.add id loc envs, pc')
    in
    if (List.length args) != (List.length arg_ids) then
      arity_mismatch_err p arg_ids args pc
    else
      (* Push the closed-over env onto the env stack. *)
      let envs = Env.stack_push closure_env envs in
      let (envs_args, pc_args) = List.fold_right2 bind_arg args arg_ids (envs, pc) in
      nested_eval body_exp envs_args pc_args
  | ObjPtr obj_loc ->
    begin match sto_lookup_obj obj_loc pc with
    | ConObj { attrs = { code = Some cloc }}
    | SymObj ({ attrs = { code = Some cloc }}, _) ->
      apply p (sto_lookup_val cloc pc) args envs pc nested_eval
    | NewSymObj _ -> failwith (interp_error p ("Apply got NewSymObj"))
    | o -> throw_str (interp_error p
                        ("Applied an object without a code attribute: "
                        ^ obj_to_string (o, false))) pc
    end
  | SymScalar id -> throw_str (interp_error p
                                 ("Tried to apply a symbolic value: " ^ id)) pc
  | _ -> failwith (interp_error p 
                     ("Applied non-function, was actually " ^ 
                         val_to_string func))

(* Creates all possible branches for a symbolic boolean value,
 * returning results whose values are OCaml bools. *)
let branch_bool exp sym_bool pc = match sym_bool with
  | BTrue -> return true pc
  | BFalse -> return false pc
  | BSym id -> combine
    (add_trace_pt (exp, "symbool true")
      (return true (fst (add_assert (SCastJS (TBool, SId id)) pc))))
    (add_trace_pt (exp, "symbool false")
      (return false (fst (add_assert (SNot (SCastJS (TBool, SId id))) pc))))

let branch_string sym_str pc = match sym_str with
  | SString str -> return (String str) pc
  (* TODO We lose type info here, although Z3 should know its a string already. Need to
   * examine use cases and figure out what to do here. *)
  | SSym id -> return (SymScalar id) pc

(* If given a NewSym, initializes it, creating a branch for
 * every possible value it could be (either an ObjPtr to every
 * object that was in the store when it was created, or a SymScalar). *)
let branch_sym exp v pc = 
  match v with
  | NewSym (id, obj_locs) ->
    let branch newval pc =
      add_trace_pt (exp, id ^ " -> " ^ val_to_string newval)
        (return newval
          (* Update every location in the store that has a NewSym
           * with the same id, since that sym value has now been init'd *)
          (add_hint
            ("Replaced NewSym " ^ id ^ " with " ^ val_to_string newval)
            (S.pos_of exp)
            { pc with store =
              { pc.store with vals =
                Store.mapi
                  (fun loc v -> match v with
                  | NewSym (id', _) -> if id' = id then newval else v
                  | _ -> v)
                  pc.store.vals }}))
    in
    combine
      (* One branch for if its a scalar *)
      (branch (SymScalar id) pc)
      (* One branch for each object it could point to *)
      (List.fold_left
         (fun branches obj_loc ->
           combine (branch (ObjPtr obj_loc) pc) branches)
         none obj_locs)
  | _ -> return v pc

let get_conps { conps = conps } = conps
let get_symps { symps = symps } = symps
let set_conps o conps = { o with conps = conps }
let set_symps o symps = { o with symps = symps }

let rec set_prop loc o f prop pc = match o, f with
  | SymObj (o, locs), SymField f -> (SymObj (set_symps o (IdMap.add f prop (get_symps o)), locs), pc)
  | SymObj (o, locs), ConField f -> (SymObj (set_conps o (IdMap.add f prop (get_conps o)), locs), pc)
  | ConObj o, SymField f -> (ConObj (set_symps o (IdMap.add f prop (get_symps o))), pc)
  | ConObj o, ConField f -> (ConObj (set_conps o (IdMap.add f prop (get_conps o))), pc)
  | NewSymObj locs, _ ->
    let o, pc = init_sym_obj locs loc "init_sym_obj set_prop" pc in
    set_prop loc o f prop pc

let get_prop o f = match f with 
  | SymField f -> IdMap.find f (get_symps o)
  | ConField f -> IdMap.find f (get_conps o)

let delete_prop o f = match o, f with 
  | SymObj (o, locs), SymField f -> SymObj (set_symps o (IdMap.remove f (get_symps o)), locs)
  | SymObj (o, locs), ConField f -> SymObj (set_conps o (IdMap.remove f (get_conps o)), locs)
  | ConObj o, SymField f -> ConObj (set_symps o (IdMap.remove f (get_symps o)))
  | ConObj o, ConField f -> ConObj (set_conps o (IdMap.remove f (get_conps o)))
  | NewSymObj _, _ -> failwith "Impossible! Should have init'd NewSymObj before delete_prop"

let rec add_field_helper force exp obj_loc field newval pc = 
  match sto_lookup_obj obj_loc pc with
  | ((SymObj ({ attrs = { extensible = symext; }}, _)) as o)
  | ((ConObj { attrs = { extensible = symext; }}) as o) ->
    bind
      (if force then (* Create a new sym prop *)
        (* TODO : Create Accessor fields once we figure out how to handle symbolic
        * functions *)
        let vloc, pc = sto_alloc_val newval pc in
        let symwrit, pc = new_sym_bool "writable" "add_field writable" pc in
        let symenum, pc = new_sym_bool "enum" "add_field enum" pc in
        let symconf, pc = new_sym_bool "config" "add_field config" pc in
        return (Some (Data ({ value = vloc; writable = symwrit; }, symenum, symconf))) pc
      else (* Check extensible and create a new concrete prop *)
        bind (branch_bool exp symext pc)
          (fun (ext, pc) -> 
            if not ext then return None pc else
              let vloc, pc = sto_alloc_val newval pc in
              return (Some (Data ({ value = vloc; writable = BTrue; }, BTrue, BTrue))) pc))
      (* Add the prop that we got from above *)
      (fun (new_prop, pc) ->
        match new_prop with
        | None -> return (field, None, Undefined) pc
        | Some new_prop ->
          let new_obj, pc = set_prop obj_loc o field new_prop pc in
          return (field, Some new_prop, newval)
            (sto_update_obj obj_loc new_obj pc))
  | NewSymObj locs ->
    let _, pc = init_sym_obj locs obj_loc "init_sym_obj add_field" pc in
    add_field_helper force exp obj_loc field newval pc

let add_field exp loc field v pc = bind (add_field_helper false exp loc field v pc)
  (fun ((_, _, v), pc) -> return v pc)

let add_field_force exp loc field v pc = bind (add_field_helper true exp loc field v pc)
  (fun ((field, prop, _), pc) -> return (field, prop) pc)

(* (\* Both functions (because a property can satisfy writable and not_writable) *\) *)
(* let rec writable prop = match prop with *)
(*   | Data ({ writable = true; }, _, _) -> true *)
(*   | _ -> false *)

(* let rec not_writable prop = match prop with *)
(*   | Data ({ writable = false; }, _, _) -> true *)
(*   | _ -> false *)


let branch_bool_wrap exp sym_bool pc =
  bind (branch_bool exp sym_bool pc)
    (fun (b, pc) -> return (bool b) pc)

let get_obj_attr exp oattr attrs pc = match attrs, oattr with
  | { proto=ploc }, S.Proto -> return (sto_lookup_val ploc pc) pc
  | { extensible=sym_ext}, S.Extensible -> branch_bool_wrap exp sym_ext pc
  | { code=Some cloc}, S.Code -> return (sto_lookup_val cloc pc) pc
  | { code=None}, S.Code -> return Null pc
  | { primval=Some pvloc}, S.Primval -> return (sto_lookup_val pvloc pc) pc
  | { primval=None}, S.Primval -> throw_str "[interp] Got Primval attr of None" pc
  | { klass=sym_klass }, S.Klass -> branch_string sym_klass pc

let set_obj_attr oattr obj_loc newattr pc =
  let update_attrs o wrap_obj =
    let set_attrs newattrs pc =
      let new_obj = wrap_obj { o with attrs = newattrs } in
      let pc = sto_update_obj obj_loc new_obj pc in
      return newattr pc
    in
    match oattr, newattr with
    | S.Proto, ObjPtr _
    | S.Proto, Null ->
      let ploc, pc = sto_alloc_val newattr pc in
      set_attrs { o.attrs with proto=ploc } pc
    | S.Proto, _ ->
      throw_str ("[interp] Update proto given invalid value: "
                 ^ val_to_string newattr) pc
    | S.Extensible, True -> set_attrs { o.attrs with extensible=BTrue } pc
    | S.Extensible, False -> set_attrs { o.attrs with extensible=BFalse } pc
    | S.Extensible, SymScalar id -> set_attrs { o.attrs with extensible=BSym id } pc
    | S.Extensible, _ ->
      throw_str ("[interp] Update extensible given invalid value: "
                 ^ val_to_string newattr) pc
    | S.Code, _ -> throw_str "[interp] Can't update Code" pc
    | S.Primval, _ -> throw_str "[interp] Can't update Primval" pc
    | S.Klass, _ -> throw_str "[interp] Can't update Klass" pc
  in
  match sto_lookup_obj obj_loc pc with
  | ConObj o -> update_attrs o (fun o -> ConObj o) 
  | SymObj (o, locs) -> update_attrs o (fun o -> (SymObj (o, locs)))
  | NewSymObj _ -> failwith "Impossible! Should have init'd NewSymObj."

(* Gets an attr of a prop of an object *)
(*let get_attr attr props field pc = *)
(*  if (not (IdMap.mem field props)) then undef*)
let get_attr exp attr prop pc = match prop, attr with
| Data (_, _, sym_conf), S.Config
| Accessor (_, _, sym_conf), S.Config -> branch_bool_wrap exp sym_conf pc
| Data (_, sym_enum, _), S.Enum
| Accessor (_, sym_enum, _), S.Enum -> branch_bool_wrap exp sym_enum pc
| Data ({ writable = sym_writ; }, _, _), S.Writable -> branch_bool_wrap exp sym_writ pc
| Data ({ value = vloc; }, _, _), S.Value -> return (sto_lookup_val vloc pc) pc
| Accessor ({ getter = gloc; }, _, _), S.Getter -> return (sto_lookup_val gloc pc) pc
| Accessor ({ setter = sloc; }, _, _), S.Setter -> return (sto_lookup_val sloc pc) pc
| _ -> failwith "bad access of attribute"

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
let set_attr exp attr obj_loc field prop newval pc =
  let objv = sto_lookup_obj obj_loc pc in
  let set_new_prop newprop pc =
    let new_obj, pc = set_prop obj_loc objv field newprop pc in
    return True (sto_update_obj obj_loc new_obj pc)
  in
  bind
    (match objv with
    | ConObj { attrs = { extensible = ext; } }
    | SymObj ({ attrs = { extensible = ext; } }, _) -> branch_bool exp ext pc
    | NewSymObj _ -> failwith "Impossible! set_attr given NewSymObj")
    (fun (ext, pc) -> match prop with
    | None ->
      if ext then match attr with
      | S.Getter ->
        let vloc, pc = sto_alloc_val newval pc in 
        let uloc, pc = sto_alloc_val Undefined pc in
        set_new_prop (Accessor ({ getter = vloc; setter = uloc; },
                  BFalse, BFalse)) pc
      | S.Setter ->
        let vloc, pc = sto_alloc_val newval pc in 
        let uloc, pc = sto_alloc_val Undefined pc in
        set_new_prop (Accessor ({ getter = uloc; setter = vloc; },
                  BFalse, BFalse)) pc
      | S.Value ->
        let vloc, pc = sto_alloc_val newval pc in 
        set_new_prop (Data ({ value = vloc; writable = BFalse; }, BFalse, BFalse)) pc
      | S.Writable ->
        let uloc, pc = sto_alloc_val Undefined pc in
        set_new_prop (Data ({ value = uloc; writable = symboolv newval },
              BFalse, BFalse)) pc
      | S.Enum ->
        let uloc, pc = sto_alloc_val Undefined pc in
        set_new_prop (Data ({ value = uloc; writable = BFalse },
              symboolv newval, BTrue)) pc
      | S.Config ->
        let uloc, pc = sto_alloc_val Undefined pc in
        set_new_prop (Data ({ value = uloc; writable = BFalse },
              BTrue, symboolv newval)) pc
      else
        failwith "[interp] Extending inextensible object ."
    | Some prop -> begin
      (* 8.12.9: "If a field is absent, then its value is considered
         to be false" -- we ensure that fields are present and
         (and false, if they would have been absent). *)
      match prop, attr, newval with
        (* S.Writable true -> false when configurable is false *)
        | Data ({ writable = BTrue } as d, enum, config), S.Writable, new_w ->
          set_new_prop (Data ({ d with writable = symboolv new_w }, enum, config)) pc
        | Data (d, enum, BTrue), S.Writable, new_w ->
          set_new_prop (Data ({ d with writable = symboolv new_w }, enum, BTrue)) pc
        (* Updating values only checks writable *)
        | Data ({ writable = BTrue } as d, enum, config), S.Value, v ->
          let vloc, pc = sto_alloc_val v pc in
          set_new_prop (Data ({ d with value = vloc }, enum, config)) pc
        (* If we had a data property, update it to an accessor *)
        | Data (d, enum, BTrue), S.Setter, setterv ->
          let sloc, pc = sto_alloc_val setterv pc in
          let uloc, pc = sto_alloc_val Undefined pc in
          set_new_prop (Accessor ({ getter = uloc; setter = sloc }, enum, BTrue)) pc
        | Data (d, enum, BTrue), S.Getter, getterv ->
          let gloc, pc = sto_alloc_val getterv pc in
          let uloc, pc = sto_alloc_val Undefined pc in
          set_new_prop (Accessor ({ getter = gloc; setter = uloc }, enum, BTrue)) pc
        (* Accessors can update their getter and setter properties *)
        | Accessor (a, enum, BTrue), S.Getter, getterv ->
          let gloc, pc = sto_alloc_val getterv pc in
          set_new_prop (Accessor ({ a with getter = gloc }, enum, BTrue)) pc
        | Accessor (a, enum, BTrue), S.Setter, setterv ->
          let sloc, pc = sto_alloc_val setterv pc in
          set_new_prop (Accessor ({ a with setter = sloc }, enum, BTrue)) pc
        (* An accessor can be changed into a data property *)
        | Accessor (a, enum, BTrue), S.Value, v ->
          let vloc, pc = sto_alloc_val v pc in
          set_new_prop (Data ({ value = vloc; writable = BFalse; }, enum, BTrue)) pc
        | Accessor (a, enum, BTrue), S.Writable, w ->
          let uloc, pc = sto_alloc_val Undefined pc in
          set_new_prop (Data ({ value = uloc; writable = symboolv w; }, enum, BTrue)) pc
        (* enumerable and configurable need configurable=true *)
        | Data (d, _, BTrue), S.Enum, new_enum ->
          set_new_prop (Data (d, symboolv new_enum, BTrue)) pc
        | Data (d, enum, BTrue), S.Config, new_config ->
          set_new_prop (Data (d, enum, symboolv new_config)) pc
        | Data (d, enum, BFalse), S.Config, False ->
          set_new_prop (Data (d, enum, BFalse)) pc
        | Accessor (a, enum, BTrue), S.Config, new_config ->
          set_new_prop (Accessor (a, enum, symboolv new_config)) pc
        | Accessor (a, enum, BTrue), S.Enum, new_enum ->
          set_new_prop (Accessor (a, symboolv new_enum, BTrue)) pc
        | Accessor (a, enum, BFalse), S.Config, False ->
          set_new_prop (Accessor (a, enum, BFalse)) pc
        | _ ->
          let fstr = match field with ConField f | SymField f -> f in
          throw_str (interp_error (Ljs_syntax.pos_of exp) ("Can't set attr "
            ^ Ljs_syntax.string_of_attr attr
            ^ " to "
            ^ val_to_string newval
            ^ " for field: " ^ fstr)) pc
            (*^ (FormatExt.to_string (curry prop)) (fstr, prop))) pc*)
      end)

(* 8.10.5, steps 7/8 "If iscallable(getter) is false and getter is not
   undefined..." *)

(* and fun_obj value = match value with *)
(*   | ObjPtr c -> begin match !c with *)
(*       | { code = Some (Closure cv) }, _ -> true *)
(*       | _ -> false *)
(*   end *)
(*   | Undefined -> false *)
(*   | _ -> false *)
    
let check_field field pc = 
  match field with
  | String f    -> return (ConField f) pc
  | SymScalar f -> return (SymField f) pc
  | _ -> throw_str ("get_field called with non-string/sym field: "
                    ^ val_to_string field) pc

(* sym_get_prop takes a field name and a pointer to an object and looks up the property
 * at that field on the object. The field and the object may each by concrete or
 * symbolic, which means we have to use our super secret algorithm, which, with the gory
 * details omitted, is below.
 *
 * (apologies for the odd conflation of "field" and "property" - let's just say that
 * fields are the keys, properties are the values.
 *
 * Invariant on the concrete and symbolic field maps in an object:
 *    Every field name in either map is distinct (in the Z3 sense)
 *    from all other field names in both maps.
 *
 * Let F be the field, O be the object, prop(X) be the prop at field x
 * If O is null, return None
 * If F is concrete (a string)
 *  - If F is in the concrete fields, return prop(F)
 *  - Otherwise, we branch:
 *    1. For each F' in sym fields, create a branch, assert F = F', and return prop(F')
 *    2. Create a branch, assert for all F' in sym fields, F <> F',
 *       and recurse on the prototype of O
 *    3. If O symbolic, create a branch, assert for all F' in sym fields, F <> F',
 *       add a prop P containing a new sym val to the con fields of O at F, return P 
 * If F is symbolic (hopefully a string)
 *  - If F is in the symbolic fields, return prop(F)
 *  - Otherwise, we branch:
 *    1. For each F' in concrete or sym fields, create a branch,
 *       assert F = F', and return prop(F')
 *    2. Create a branch, assert for all F' in concrete or sym fields, F <> F',
 *       and recurse on the prototype of O
 *    3. If O symbolic, create a branch, assert for all F' in concrete or
 *       sym fields, F <> F', add a prop P containing a new sym val to the
 *       sym fields of O at F, return P 
 *
 * Note that the two cases are nearly symmetrical, but when F is concrete, we don't
 * need to check the cases where F is asserted equal to some other concrete field,
 * since we know those are going to be unsat.
 *
 * We check sat when creating each branch, naturally.
 *
 * sym_get_own_prop is the same, without checking the proto.
 *)
let rec sym_get_prop_helper check_proto sym_proto_depth exp pc obj_ptr field =
  match obj_ptr with
  | NewSym (id, locs) -> failwith "Impossible"
  | SymScalar id -> return (field, None) (fst (add_assert (is_null_sym id) pc))
  | Null -> return (field, None) pc
  | ObjPtr obj_loc -> 
    let helper objv is_sym sym_locs pc =
      let { attrs = { proto = ploc; }; conps = conps; symps = symps} = objv in
      let potential_props = begin
        try return (field, Some (get_prop objv field)) pc
        with Not_found -> 
          let fstr, pc = field_str field pc in
          let prop_branches wrap_f props = IdMap.fold
            (fun f' v' branches ->
              let field' = wrap_f f' in
              let f'str, pc = field_str field' pc in
              let pc, unchanged = add_assert (is_equal (SId fstr) (SId f'str)) pc in
              let new_branch =
                if unchanged || is_sat pc
                     ("get_prop " ^ val_to_string obj_ptr ^ " at " ^ fstr)
                then return (field', Some v') pc
                else unsat pc in
              let new_branch = add_trace_pt (exp, fstr ^ " = " ^ f'str) new_branch in
              combine new_branch branches)
            props none
          in
          let con_branches = match field with
          | ConField f -> none
          | SymField f -> prop_branches (fun f -> ConField f) conps
          in
          let branches = combine con_branches (prop_branches (fun f -> SymField f) symps) in
          let assert_neq wrap_f =
            (fun f' _ (pc, all_unchanged) ->
              let f'str, pc = field_str (wrap_f f') pc in
              let pc, unchanged =
                add_assert (is_not_equal (SId fstr) (SId f'str)) pc in
              (pc, unchanged && all_unchanged)) in
          let none_pc, unchanged = IdMap.fold (assert_neq (fun f -> SymField f)) symps (pc, true) in
          let none_pc, unchanged = match field with
          | ConField f -> none_pc, unchanged
          | SymField f -> IdMap.fold (assert_neq (fun f -> ConField f)) conps (none_pc, unchanged)
          in
          let none_branch = 
            if not (unchanged || is_sat none_pc ("get_prop none " ^ val_to_string obj_ptr))
            then unsat none_pc
            else
              if check_proto && (not is_sym || sym_proto_depth > 0) then
                bind (branch_sym exp (sto_lookup_val ploc none_pc) none_pc)
                  (fun (protov, pc) ->
                    let sym_proto_depth' =
                      if is_sym
                      then sym_proto_depth - 1
                      else sym_proto_depth in
                    sym_get_prop_helper check_proto sym_proto_depth' exp pc protov field) 
              else return (field, None) pc in
          (* Only add a trace if we actually did some branching *)
          let none_branch = if branches = none then none_branch else
            add_trace_pt (exp, fstr ^ " <> all fields") none_branch in
          combine none_branch branches
      end in
      bind potential_props (fun ((field, prop), pc) ->
        match prop with
        | None -> 
          (* Case 3.
           * If it's possible that the property wasn't found, then
           * if this obj is symbolic, the property *might* have existed on this obj, 
           * or it might never have existed, so return both None and the new prop (and
           * add the new prop to the object) *)
          let not_found_branch = return (field, None) pc in
          let added_prop_branch_opt = 
            if is_sym then 
              let locs = match sym_locs with
              | None -> failwith "Should have sym_locs if is_sym true"
              | Some locs -> locs in
              let symv, pc = new_sym_from_locs locs ""
                ("sym_get_prop at " ^ (Pos.string_of_pos (S.pos_of exp))) pc in
              Some (add_field_force exp obj_loc field symv pc)
            else None
          in
          (* We only want to add trace pts if we have multiple branches. *)
          begin match added_prop_branch_opt with
            | Some added_prop_branch ->
              combine
                (add_trace_pt (exp, "prop not found") not_found_branch)
                (add_trace_pt (exp, "prop not found, added to obj") added_prop_branch)
            | None -> not_found_branch
          end
        | Some p -> return (field, prop) pc)
    in begin match sto_lookup_obj obj_loc pc with
    | ConObj o -> helper o false None pc
    | SymObj (o, locs) -> helper o true (Some locs) pc
    | NewSymObj locs ->
      let _, pc = (init_sym_obj locs obj_loc "init_sym_obj sym_get_prop" pc) in
      sym_get_prop_helper check_proto sym_proto_depth exp pc obj_ptr field
    end
  | _ -> throw_str (interp_error (S.pos_of exp)
         "get_prop on a non-object.  The expression was (get-prop " 
       ^ val_to_string obj_ptr
       ^ " " ^ fst (field_str field pc) ^ ")") pc
let sym_get_prop = sym_get_prop_helper true max_sym_proto_depth
let sym_get_own_prop = sym_get_prop_helper false max_sym_proto_depth

let rec eval jsonPath maxDepth depth exp (envs : Env.stack) (pc : ctx) : results = 
  (* printf "In eval %s %d %d %s\n" jsonPath maxDepth depth *)
  (*   (Ljs_pretty.exp exp Format.str_formatter; Format.flush_str_formatter()); *)
  if (not pc.hide_objs) && print_store then printf "%s\n" (store_to_string pc.store);

  if (depth >= maxDepth)
  then throw_str ("Reached max recursion depth " ^ (string_of_int maxDepth)) pc else 

  let nested_eval = eval jsonPath maxDepth (depth + 1) in
  let eval = eval jsonPath maxDepth depth in 
  (* eval_sym should be called to eval an expression e that is part of an expression
   * which determines whether e is a scalar or a pointer. For instance, the expression
   * e + 1 means e must be a scalar. But the expression e[0] means that e is a pointer.
   * In either case, eval_sym should be used to evaluate e. *)
  let eval_sym exp envs pc = bind (eval exp envs pc) (uncurry (branch_sym exp)) in

  let pc = { pc with print_env = (Env.stack_curr envs); } in

  match exp with
    | S.Hint (p, hint, exp2) ->
      printf "%s : %b\n" hint (string_match summarize_regex hint 0);
      if summarize_functions && string_match summarize_regex hint 0
      then
        let func_name = matched_group 1 hint in
        printf "going to summarize %s\n" func_name;
        Ljs_sym_summary.make_summary func_name eval (apply p) envs pc;
        eval exp2 envs pc
      else
        eval exp2 envs begin
          if hint = start_sym_keyword
          then { pc with hide_objs = false }
          else if hint = stop_sym_keyword
          then { pc with hide_objs = true }
          else pc
    end

    | S.Undefined _ -> return Undefined pc 
    | S.Null _ -> return Null pc 
    | S.String (_, s) -> return (String s) (add_const_string s pc)
    | S.Num (_, n) -> return (Num n) pc
    | S.True _ -> return True pc
    | S.False _ -> return False pc
    | S.Id (p, x) -> begin
      (* This catches new syms in handwritten LJS, but not desugared JS.
       * Desugared JS new syms are caught in GetField. *)
      if x = new_sym_keyword then begin
        let pc = do_gc envs pc in
        uncurry return
          (new_sym (new_sym_keyword ^ " at " ^ (Pos.string_of_pos p)) pc)
      end
      else if x = fresh_sym_keyword then
        uncurry return
          (new_sym_fresh (fresh_sym_keyword ^ " at " ^ (Pos.string_of_pos p)) pc)
      else
        try return (sto_lookup_val (Env.lookup x envs) pc) pc
        with Not_found -> failwith (interp_error p
          ("Unbound identifier: " ^ x ^ ", " ^ Store.print_loc (Env.lookup x envs) ^ " in identifier lookup at "
           ^ Pos.string_of_pos p))
    end


    | S.Lambda (p, xs, e) ->
      (* Close over the current env on the env stack *)
      return (Closure (xs, e, Env.stack_curr envs)) pc

    | S.Op1 (p, op, e) ->
      bind 
        (eval_sym e envs pc)
        (fun (ev, pc) -> 
          try
            match ev with
            | SymScalar id -> 
              let (t,ret_ty) = typeofOp1 op in 
              let pc = check_type id t pc in
              let (ret_op1, pc) = fresh_var ("P1_" ^ op ^ "_") ret_ty
                ("return from " ^ op ^ " " ^ Pos.string_of_pos p) pc in
              return (SymScalar ret_op1)
                (add_let ret_op1 (SOp1 (op, SId id)) pc)
            | ObjPtr obj_loc ->
              begin match sto_lookup_obj obj_loc pc with
              | NewSymObj locs ->
                let _, pc = init_sym_obj locs obj_loc "op1 init_sym_obj" pc in
                op1 pc op ev
              | _ -> op1 pc op ev
              end
            | _ -> op1 pc op ev
          with
          | PrimError msg -> throw_str msg pc
          | TypeError _ -> none)
        
    | S.Op2 (p, op, e1, e2) -> 
      bind
        (eval_sym e1 envs pc)
        (fun (e1_val, pc) ->
          bind 
            (eval_sym e2 envs pc)
            (fun (e2_val, pc) -> 
              (* Special case for op2s on objects *)
              match op with
              | "hasProperty" -> 
                (* In desugared JS, hasProperty is called on the global object
                 * for our special keywords and we need to fake it returning true. *)
                begin match e2_val with
                | String fstr when fstr = new_sym_keyword
                                || fstr = fresh_sym_keyword -> return True pc
                | _ ->

                bind (check_field e2_val pc)
                  (fun (field, pc) ->
                    bind (sym_get_prop exp pc e1_val field)
                      (fun ((_, prop), pc) ->
                         return (bool (prop <> None)) pc))
                end
              | "hasOwnProperty" ->
                bind (check_field e2_val pc)
                  (fun (field, pc) ->
                    bind (sym_get_own_prop exp pc e1_val field)
                      (fun ((_, prop), pc) ->
                         return (bool (prop <> None)) pc))
              | "isAccessor" ->
                bind (check_field e2_val pc)
                  (fun (field, pc) ->
                    bind (sym_get_prop exp pc e1_val field)
                      (fun ((_, prop), pc) -> 
                        return (bool (match prop with
                          Some (Accessor _) -> true | _ -> false)) pc))
              | _ -> begin
                match e1_val, e2_val with
                | SymScalar _, SymScalar _
                | SymScalar _, _
                | _, SymScalar _ -> begin 
                  let t1, t2, ret_ty = typeofOp2 op in
                  try 
                    let sym_e1, pc = match e1_val with
                      | SymScalar id -> (SId id, check_type id t1 pc)
                      | _ -> (Concrete e1_val, pc) in
                    let sym_e2, pc = match e2_val with
                      | SymScalar id -> (SId id, check_type id t2 pc)
                      | _ -> (Concrete e2_val, pc) in
                    (* Special case for stx=, result depends both on
                     * vals being equal and types of vals being equal *)
                    let res_exp = if op = "stx="
                      then SUncastJS (TBool, SAnd [
                        is_equal sym_e1 sym_e2;
                        is_equal (SOp1("typeof", sym_e1)) (SOp1("typeof", sym_e2))
                      ])
                      else SOp2(op, sym_e1, sym_e2)
                    in
                    let (res_var, pc) = fresh_var ("P2_" ^ op ^ "_") ret_ty
                      ("return from " ^ op ^ " " ^ Pos.string_of_pos p) pc in
                    return (SymScalar res_var) (add_let res_var res_exp pc)
                  with TypeError id -> none 
                end
                | _ -> 
                  try
                    let (ret, pc) = op2 pc op e1_val e2_val in
                    return ret pc
                  with PrimError msg -> throw_str msg pc
              end))
        
    | S.If (p, c, t, f) ->
      bind 
        (eval_sym c envs pc)
        (fun (c_val, pc') -> 
          match c_val with
          | True -> eval t envs pc'
          | SymScalar id -> 
            combine
              (add_trace_pt (exp, "if true")
                (let (true_pc, unchanged) =
                   add_assert (SCastJS (TBool, SId id)) pc' in
                 let true_pc = add_hint ("if true") p true_pc in
                 if unchanged || is_sat true_pc ("if " ^ val_to_string c_val ^ " true")
                 then eval t envs true_pc
                 else unsat true_pc))
              (add_trace_pt (exp, "if false")
                (let (false_pc, unchanged) =
                   add_assert (SNot (SCastJS (TBool, SId id))) pc' in
                 let false_pc = add_hint ("if false") p false_pc in
                 if unchanged || is_sat false_pc ("if " ^ val_to_string c_val ^ " false")
                 then eval f envs false_pc
                 else unsat false_pc))
          (* TODO should ObjPtr's be truthy? *)
          | _ -> eval f envs pc')
        
    | S.App (p, f, args) -> 
      let f_vals = eval_sym f envs pc in
      let nested_eval =
        if summarize_functions
        then match Ljs_sym_summary.get_summary f with
          | Some apply_summary -> apply_summary
          | None -> nested_eval
        else nested_eval
      in
      bind f_vals
        (fun (f_val, pc_f) ->
          bind 
            (List.fold_left 
              (fun avpcs arg ->
                bind avpcs
                  (fun ((argvs : value list), (pcs : ctx)) -> 
                    bind 
                      (* We don't need to eval_sym the args because
                       * they will be rebound in apply *)
                      (eval arg envs pcs)
                      (fun (argv, pcs') ->
                        return (argvs @ [argv]) pcs')))
              (return [] pc_f) args)
            (fun (argvs, pcs) ->
              match f_val with
              | SymScalar f -> 
                let ((fid : string), (fpc : ctx)) = fresh_var "F" (TFun (List.length argvs)) "function to be applied" pcs in
                let (argvs : sym_exp list) = List.map val_sym argvs in
                let ((vs : sym_exp list), (pcs' : ctx)) = List.fold_left
                  (fun (vals, p) exp -> (vals@[exp], p))
                  ([], fpc) argvs in
                let (res_id, res_pc) = fresh_var "AP" TAny "result of function call" pcs' in 
                return (SymScalar res_id)
                  (add_let res_id (SApp (SId fid, vs))
                     (add_let fid (SId f) res_pc))
              | _ -> apply p f_val argvs envs pcs nested_eval))
        
    | S.Seq (p, e1, e2) -> 
      bind 
        (eval e1 envs pc) 
        (fun (_, pc') -> eval e2 envs pc')

    | S.Let (p, x, e, body) ->
      bind
        (eval e envs pc)
        (fun (e_val, pc) -> 
          let loc, pc = sto_alloc_val e_val pc in 
          eval body (Env.add x loc envs) pc)
        
    | S.Rec (p, x, e, body) ->
      let (loc, pc') = sto_alloc_val Undefined pc in
      let envs' = Env.add x loc envs in
      bind
        (eval e envs' pc')
        (fun (ev_val, pc') -> 
          let pc'' = sto_update_val loc ev_val pc' in 
          eval body envs' pc'')

    | S.SetBang (p, x, e) -> begin
      try
        let loc = Env.lookup x envs in
        bind 
          (eval e envs pc)
          (fun (e_val, pc') ->
            let pc'' = sto_update_val loc e_val pc' in
            return e_val pc'')
      with Not_found ->
        failwith ("[interp] Unbound identifier: " ^ x ^ " in set! at " ^
                     (Pos.string_of_pos p))
    end

    | S.Object (p, attrse, propse) -> begin
      match attrse with
      | { S.primval = valexp;
          S.proto = protoexp;
          S.code = codexp;
          S.extensible = ext;
          S.klass = kls; } ->

        let opt_lift ctxs = 
          bind ctxs
            (fun (v, pc) -> 
              let (vloc, pc) = sto_alloc_val v pc in
              return (Some vloc) pc) in
        bind
          (match valexp with
          | None -> return None pc
          | Some vexp -> opt_lift (eval vexp envs pc))
          (fun (vloc, pc_v) ->
            bind
              (match protoexp with
              | None -> return Undefined pc_v
              | Some pexp -> eval_sym pexp envs pc_v)
              (fun (p, pc_p) ->
                let (ploc, pc_p) = sto_alloc_val p pc_p in
                bind
                  (match codexp with
                  | None -> return None pc_p
                  | Some cexp -> opt_lift (eval cexp envs pc_p))
                  (fun (cloc, pc_c) ->
                    let attrsv =
                      { primval = vloc; proto = ploc; code = cloc;
                        extensible = symbool ext; klass = SString kls }
                    in
                    let eval_prop prop pc = match prop with
                      | S.Data ({ S.value = vexp; S.writable = w; }, enum, config) ->
                        bind (eval vexp envs pc)
                          (fun (v2, pc_v2) ->
                            let v2loc, pc_v2 = sto_alloc_val v2 pc_v2 in
                            return (Data ({ value = v2loc; writable = symbool w; },
                                          symbool enum, symbool config)) pc_v2)
                      | S.Accessor ({ S.getter = ge; S.setter = se; }, enum, config) ->
                        bind (eval ge envs pc)
                          (fun (v2, pc_v2) ->
                            let v2loc, pc_v2 = sto_alloc_val v2 pc_v2 in
                            bind (eval se envs pc_v2)
                              (fun (v3, pc_v3) ->
                                let v3loc, pc_v3 = sto_alloc_val v3 pc_v3 in
                                return (Accessor ({ getter = v2loc; setter = v3loc},
                                                  symbool enum, symbool config)) pc_v3))
                    in
                    bind
                      (List.fold_left
                        (fun maps_pcs (name, prop) -> 
                          bind maps_pcs
                            (fun (map, pc) ->
                              bind 
                                (eval_prop prop pc)
                                (fun (propv, pc_v) -> 
                                  let (_, pc') = const_string name pc_v in
                                  return (IdMap.add name propv map) pc')))
                        (return IdMap.empty pc_c) propse)
                      (fun (propvs, pc_psv) -> 
                        let objv = ConObj { attrs = attrsv; conps = propvs; symps = IdMap.empty; } in
                        let (loc, pc_obj) = sto_alloc_obj objv pc_psv in
                        return (ObjPtr loc) pc_obj))))
    end
      
    (* GetAttr gets the specified attr of one property of an object, as opposed to
     * getting an attr of the object itself. *)
    | S.GetAttr (p, attr, obj_ptr, field) ->
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc_o) -> 
          bind (eval_sym field envs pc_o) 
            (fun (fv, pc_f) -> 
              bind (check_field fv pc_f)
                 (fun (fv, pc') -> 
                   (* get own prop since we shouldn't check proto *)
                   bind (sym_get_own_prop exp pc' obj_ptrv fv)
                     (fun ((_, prop_opt), pc') -> match prop_opt with
                     | Some prop -> get_attr exp attr prop pc'
                     | None -> return Undefined pc'))))

    | S.SetAttr (p, attr, obj_ptr, field, newval) ->
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc_o) -> 
          bind (eval_sym field envs pc_o) 
            (fun (fv, pc_f) -> 
              bind (eval newval envs pc_f)
                (fun (newvalv, pc_v) ->
                  bind (check_field fv pc_v)
                    (fun (fv, pc') -> 
                      (* get own prop since we shouldn't check proto *)
                      bind (sym_get_own_prop exp pc' obj_ptrv fv)
                        (fun ((field, prop), pc') -> 
                          match obj_ptrv with
                          | ObjPtr obj_loc -> set_attr exp attr obj_loc field prop newvalv pc'
                          | SymScalar id -> throw_str "SetAttr given SymScalar" pc
                          | Null -> throw_str "SetAttr given Null" pc
                          | _ -> failwith "SetAttr given non-object")))))
                

    | S.GetObjAttr (p, oattr, obj_ptr) -> 
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc) -> 
          match obj_ptrv with
          | ObjPtr obj_loc -> begin match sto_lookup_obj obj_loc pc with
            | ConObj { attrs = attrs } -> get_obj_attr exp oattr attrs pc
            | SymObj ({ attrs = attrs }, locs) -> get_obj_attr exp oattr attrs pc
            | NewSymObj _ -> failwith "Impossible!" 
          end
          | SymScalar id -> throw_str "GetObjAttr given SymScalar" pc
          | Null -> throw_str "GetObjAttr given Null" pc
          | _ -> throw_str "GetObjAttr given non-object" pc)

    | S.SetObjAttr (p, oattr, obj_ptr, newattr) ->
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc) -> 
          bind (eval_sym newattr envs pc) (* eval_sym b/c it could be a proto *)
            (fun (newattrv, pc) ->
              match obj_ptrv with
              | ObjPtr obj_loc -> set_obj_attr oattr obj_loc newattrv pc
              | SymScalar id -> throw_str "SetObjAttr given SymScalar" pc
              | Null -> throw_str "SetObjAttr given Null" pc
              | _ -> throw_str "SetObjAttr given non-object" pc))

    | S.GetField (p, obj_ptr, f, args) -> 
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc) -> 
          bind (eval_sym f envs pc) 
            (fun (fv, pc) -> 

              (* In desugared JS, GetField is called on the global object
               * with our new sym keyword, so we catch it here to make a new sym.
               * Also need to make sure hasProperty returns true. *)
              match fv with
              | String fstr when fstr = new_sym_keyword ->
                let pc = do_gc envs pc in
                uncurry return
                  (new_sym (new_sym_keyword ^ " at " ^ (Pos.string_of_pos p)) pc)
              | String fstr when fstr = fresh_sym_keyword ->
                uncurry return
                  (new_sym_fresh (fresh_sym_keyword ^ " at " ^ (Pos.string_of_pos p)) pc)

              | _ ->

              bind (eval args envs pc)
                (fun (argsv, pc) ->
                  bind (check_field fv pc)
                    (fun (fv, pc) -> 
                      bind (sym_get_prop exp pc obj_ptrv fv)
                        (fun ((_, prop), pc) -> match prop with
                        | Some (Data ({ value = vloc; }, _, _)) ->
                          return (sto_lookup_val vloc pc) pc
                        | Some (Accessor ({ getter = gloc; }, _, _)) ->
                          let g = sto_lookup_val gloc pc in
                          apply p g [obj_ptrv; argsv] envs pc nested_eval
                        | None -> return Undefined pc)))))

    | S.SetField (p, obj_ptr, f, v, args) ->
      let update_prop obj_loc f prop newval setter_params pc = 
        let objv = sto_lookup_obj obj_loc pc in
        let unwritable = Throw (Closure ([],
          S.String (p, "Field not writable"), Env.mt_env))
        in
        match prop with
        | Some (Data ({ writable = sym_writ; }, enum, config)) ->
          bind (branch_bool exp sym_writ pc)
            (fun (writ, pc) -> 
              if writ then
                let (enum, config) =
                  (* Copied from concrete evaluator.
                   * If we found the prop on the proto,
                   * enum and config should be true *)
                  match objv with ConObj o | SymObj (o, _) -> begin
                  try let _ = get_prop o f in (enum, config)
                  with Not_found -> (BTrue, BTrue)
                  end | _ -> failwith "Impossible! update_prop shouldn't get NewSymObj"
                in
                let vloc, pc = sto_alloc_val newval pc in
                let new_obj, pc = set_prop obj_loc objv f
                  (Data ({ value = vloc; writable = BTrue }, enum, config)) pc in
                return newval (sto_update_obj obj_loc new_obj pc)
              else throw unwritable pc)
                  (*throw_str ("Can't write to non-writable field " ^ fst (field_str f pc)) pc)*)
        | Some (Accessor ({ setter = sloc; }, _, _)) -> begin
          match sto_lookup_val sloc pc with
          | Undefined -> throw unwritable pc
          | setterv -> apply p setterv setter_params envs pc nested_eval
        end
        | None -> add_field exp obj_loc f newval pc
      in
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc_o) -> 
          bind (eval_sym f envs pc_o) 
            (fun (fv, pc_f) -> 
              bind (eval v envs pc_f)
                (fun (vv, pc_v) -> 
                  bind (eval args envs pc_v)
                    (fun (argvs, pc_a) ->
                      bind (check_field fv pc_a)
                        (fun (fv, pc') -> 
                          bind (sym_get_prop exp pc' obj_ptrv fv)
                            (fun ((field, prop), pc') -> 
                              match obj_ptrv with
                              | SymScalar _ (* the SymScalar will have been asserted to be null in sym_get_prop *)
                              | Null -> return Undefined pc'
                              | ObjPtr obj_loc -> update_prop obj_loc field prop vv [obj_ptrv; argvs] pc'
                              | _ -> failwith "Impossible -- should be an ObjPtr"))))))

    | S.DeleteField (p, obj_ptr, f) ->
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc) -> 
          bind (eval_sym f envs pc) 
            (fun (fv, pc) -> 
              bind (check_field fv pc)
                (fun (fv, pc) -> 
                  (* get own prop since we don't want to check proto *)
                  bind (sym_get_own_prop exp pc obj_ptrv fv)
                    (fun ((field, prop), pc) -> 
                      match obj_ptrv with
                      | SymScalar _ (* the SymScalar will have been asserted to be null in sym_get_prop *)
                      | Null -> throw_str "DeleteField got a non-object" pc (* TODO is this right? *)
                      | ObjPtr obj_loc -> begin
                        let objv = sto_lookup_obj obj_loc pc in
                        match prop with
                        | Some (Data (_, _, BTrue))
                        | Some (Accessor (_, _, BTrue)) ->
                          let new_obj = delete_prop objv field in
                          return True (sto_update_obj obj_loc new_obj pc)
                        | _ -> return False pc
                      end
                      | _ -> failwith "Impossible -- should be an ObjPtr"))))


    | S.OwnFieldNames (p, obj_ptr) ->
      bind (eval_sym obj_ptr envs pc)
        (fun (obj_ptrv, pc) ->
          match obj_ptrv with
          | ObjPtr obj_loc ->
            begin match sto_lookup_obj obj_loc pc with
            | ConObj { conps = conps; symps = symps }
            | SymObj ({ conps = conps; symps = symps }, _) ->
              let add_name n x (m, pc) =
                let nloc, pc = sto_alloc_val n pc in
                (IdMap.add (string_of_int x)
                  (Data ({ value = nloc; writable = BFalse; }, BFalse, BFalse)) m, pc)
              in
              let con_names = IdMap.fold (fun k v l -> (String k :: l)) conps [] in
              let sym_names = IdMap.fold (fun k v l -> (SymScalar k :: l)) symps [] in
              let namelist = con_names @ sym_names in
              let props, pc =
                List.fold_right2 add_name namelist
                  (iota (List.length namelist)) (IdMap.empty, pc)
              in
              let d = float_of_int (List.length namelist) in
              let dloc, pc = sto_alloc_val (Num d) pc in
              let final_props =
                IdMap.add "length"
                  (Data ({ value = dloc; writable = BFalse; }, BFalse, BFalse))
                  props
              in
              let ploc, pc = sto_alloc_val Null pc in
              let new_loc, pc = sto_alloc_obj (ConObj {
                conps = final_props; symps = IdMap.empty;
                attrs = {
                  code = None; proto = ploc; extensible = BFalse;
                  klass = SString "LambdaJS internal"; primval = None;
                }
              }) pc in
              return (ObjPtr new_loc) pc
            | NewSymObj _ -> failwith "OwnFieldNames got a NewSymObj"
            end
          | _ -> throw_str "OwnFieldNames got a non-object" pc)


    | S.Label (p, l, e) -> begin
      bind_exn
        (eval e envs pc)
        (fun (e, pc') ->
          match e with
          | Break (l', v) -> if (l = l') then return v pc' else throw e pc'
          | _ -> throw e pc')
    end
    | S.Break (p, l, e) ->
      bind 
        (eval e envs pc)
        (fun (v, pc') -> throw (Break (l, v)) pc')
    | S.TryCatch (p, body, catch) -> begin
      bind_exn
        (eval body envs pc)
        (fun (e, pc') -> match e with
        | Throw v -> 
          bind
            (eval catch envs pc')
            (fun (c, pc'') -> apply p c [v] envs pc'' nested_eval)
        | _ -> throw e pc')
    end
    | S.TryFinally (p, body, fin) -> 
      bind_both
        (eval body envs pc)
        (fun (_, pc') -> eval fin envs pc')
        (fun (e, pc') -> 
          bind 
            (eval fin envs pc')
            (fun (_, pc'') -> throw e pc''))
    | S.Throw (p, e) -> 
      bind
        (eval e envs pc)
        (fun (v, pc') -> throw (Throw v) pc')
    (* TODO(joe): this should use ljs_desugar and not eval_op *)
    (*
      | S.Eval (p, e) ->
      match eval e envs with
      | String s -> eval_op s envs jsonPath
      | v -> v
    *)
    | S.Eval _ -> failwith "[interp] not yet implemented (Eval)"



(* This function is exactly as ridiculous as you think it is.  We read,
   parse, desugar, and evaluate the string, storing it to temp files along
   the way.  We make no claims about encoding issues that may arise from
   the filesystem.  Thankfully, JavaScript is single-threaded, so using
   only a single file works out. 

   TODO(joe): I have no idea what happens on windows. *)
and eval_op str envs jsonPath maxDepth pc = 
  let outchan = open_out "/tmp/curr_eval.js" in
  output_string outchan str;
  close_out outchan;
  let cmdstring = 
    (sprintf "%s /tmp/curr_eval.js 1> /tmp/curr_eval.json 2> /tmp/curr_evalerr.json" jsonPath) in
  ignore (system cmdstring);
  let inchan = open_in "/tmp/curr_evalerr.json" in
  let buf = String.create (in_channel_length inchan) in
  really_input inchan buf 0 (in_channel_length inchan);
  let json_err = regexp (quote "SyntaxError") in
  try
    ignore (search_forward json_err buf 0);
    throw_str "EvalError" pc
  with Not_found ->
    let ast =
      parse_spidermonkey (open_in "/tmp/curr_eval.json") "/tmp/curr_eval.json" in
      let (used_ids, exprjsd) = 
        js_to_exprjs Pos.dummy ast (Exprjs_syntax.IdExpr (Pos.dummy, "%global")) in
      let desugard = exprjs_to_ljs Pos.dummy used_ids exprjsd in
      if (Env.mem "%global" envs) then
        (Ljs_pretty.exp desugard std_formatter; print_newline ();
         eval jsonPath maxDepth 0 desugard envs pc (* TODO: which envs? *))
      else
        (failwith "no global")

let rec eval_expr expr jsonPath maxDepth pc = 
  let results =
    bind_exn
      (eval jsonPath maxDepth 0 expr Env.mt_envs pc)
      (fun (e, pc) -> match e with
      | Throw v ->
        throw_str ("Uncaught exception: " ^ message_of_throw v pc) pc
      | Break (l, v) -> throw_str ("Broke to top of execution, missed label: " ^ l) pc)
    in
    results
