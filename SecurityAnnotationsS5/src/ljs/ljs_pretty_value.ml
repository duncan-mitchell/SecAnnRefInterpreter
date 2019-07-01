open Prelude
open Ljs_values
open Ljs_pretty
open Ljs_annotation
module S = Ljs_syntax

open Format
open FormatExt

let get_1 (f, _, _) = f
let get_2 (_, s, _) = s
let get_3 (_, _, t) = t

let pretty_var_loc loc = text ("#" ^ Store.print_loc loc)
let pretty_obj_loc loc = text ("@" ^ Store.print_loc loc)

let pretty_env env =
  let pretty_bind (var, loc) = horz [text var; text "="; pretty_var_loc loc] in
  braces (vert (map pretty_bind (IdMap.bindings env)))

let pretty_value value = match value with 
  | ObjLoc loc -> pretty_obj_loc loc
  | Closure (env, args, body) ->
    vert [text "let";
          pretty_env env;
          horz [text "in func";
                parens (squish (intersperse (text ",") (map text args)))];
          braces (exp body)]
  | ClosureEnf (env, args, body) -> 
    let args = List.map (function (x, y) -> x) args in
    vert [text "let";
        pretty_env env;
        horz [text "in func";
              parens (squish (intersperse (text ",") (map text args)))];
        braces (exp body)]
  | primitive -> text (Ljs_values.pretty_value primitive)

let rec pretty_value_store v store = match v with
  | ObjLoc loc -> pretty_obj store (get_obj store loc)
  | _ -> pretty_value v

and pretty_obj store (avs, props, annotation) = 
    let proplist = IdMap.fold (fun k v l -> (k, v)::l) props [] in
      match proplist with
        | [] -> braces (pretty_attrsv avs store)
        | _ ->
          braces (vert [pretty_attrsv avs store;
                        vert (vert_intersperse (text ",")
                              (map (fun p -> pretty_prop p store) proplist))])

and pretty_attrsv ({ proto = p; code = c; extensible = b; klass = k; primval = pv } : attrsv) store =
  let proto = [horz [text "#proto:"; pretty_value p]] in
  let primval = match pv with None -> []
    | Some v -> [horz [text "#prim:"; pretty_value v]] in
  let code = match c with None -> [] 
    | Some v -> [horz [text "#code:"; pretty_value v]] in
  brackets (horzOrVert (map (fun x -> squish [x; (text ",")])
                          (primval@
                            proto@
                             code@
                             [horz [text "#class:"; text ("\"" ^ k ^ "\"")]; 
                              horz [text "#extensible:"; text (string_of_bool b)]])))

and obj_ann_str store v = match v with
  | ObjLoc loc -> 
          "<!" ^ string_of_ann (get_3 (get_obj store loc)) ^ "!>"
  | _ -> ""

and pretty_prop (f, prop) store = match prop with
  | Data ({value=v; writable=w}, enum, config) ->
    horz [text ("'" ^ f ^ "'"); text ":"; 
          braces (horzOrVert [horz [text "#value";
                                    pretty_value v;
                                    text ","]; 
                              horz [text "#writable"; text (string_of_bool w); text ","];
                              horz [text "#configurable"; text (string_of_bool config)]])]
  | Accessor ({getter=g; setter=s}, enum, config) ->
    horz [text ("'" ^ f ^ "'"); text ":"; braces (vert [horz [text "#getter";
                                          pretty_value g; text ","];
                                          horz[text "#setter";
                                               pretty_value s]])]

let string_of_value v store =
  let x = FormatExt.to_string (fun v -> pretty_value_store v store) v in
  x ^ (obj_ann_str store v)

let string_of_val_no_ann v store = 
  FormatExt.to_string (fun v -> pretty_value_store v store) v

let string_of_obj obj store =
  let x = FormatExt.to_string (fun obj -> pretty_obj store obj) obj in
  x ^ "<!" ^ string_of_ann (get_3 obj) ^ "!>"

let string_of_env env =
  FormatExt.to_string pretty_env env

(* Stores can be very large. This function avoids mapping over them,
   which tends to overflow the stack. *)
let print_store store = match store with
  | (obj_store, value_store, _) ->
    let pretty_bind printer pretty_loc (loc, value) =
      horzOrVert [horz [pretty_loc loc; text "="]; printer value] in
    let print_binding pretty_loc printer binding =
      print_endline
        (FormatExt.to_string (pretty_bind printer pretty_loc) binding) in
    let print_bindings pretty_loc printer store =
      List.iter (print_binding pretty_loc printer) (Store.bindings store) in
    print_bindings pretty_obj_loc (pretty_obj store) obj_store;
    print_bindings pretty_var_loc pretty_value value_store

let print_valid_anns store = 
  let str = string_of_ann (get_global_ann store) in
  print_string (str ^ "\n")

let print_values store =
  let pretty_binding (loc, value) =
    horzOrVert [horz [pretty_var_loc loc; text "="]; pretty_value value] in
  let print_binding binding =
    print_endline (FormatExt.to_string pretty_binding binding) in
  List.iter print_binding (Store.bindings (get_2 store))

let print_objects store =
  let pretty_binding (loc, value) =
    horzOrVert [horz [pretty_obj_loc loc; text "="]; pretty_obj store value] in
  let print_binding binding =
    print_endline (FormatExt.to_string pretty_binding binding) in
  List.iter print_binding (Store.bindings (get_1 store))
