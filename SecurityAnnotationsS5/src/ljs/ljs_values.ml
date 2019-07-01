open Prelude
open Ljs_syntax
open Ljs_annotation

open Format
open FormatExt

type env = Store.loc IdMap.t
type label = string

type value =
  | Null
  | Undefined
  | Num of float * annotation
  | String of string * annotation
  | True of annotation
  | False of annotation
      (* Objects shouldn't have VarCells in them, but can have any of
      the other kinds of values *)
  | ObjLoc of Store.loc
  | Closure of env * id list * exp
  | ClosureEnf of env * (id * annotation) list * exp
and store = (objectv Store.t * value Store.t * annotation Hierarchy.t)
and objectv = attrsv * (propv IdMap.t) * annotation
and
  attrsv = { code : value option;
             proto : value;
             extensible : bool;
             klass : string;
             primval : value option; }
and
  propv = 
  | Data of datav * bool * bool (* datav, enumerable, configurable *)
  | Accessor of accessorv * bool * bool (* accv, enumerable, configurable *)
and datav = { value : value; writable : bool; }
and accessorv = { getter : value; setter : value; }

let d_attrsv = { primval = None;
                 code = None; 
                 proto = Null; 
                 extensible = false; 
                 klass = "LambdaJS internal"; }

let get_obj (objs, _, _) loc = Store.lookup loc objs
let get_var (_, vars, _) loc = Store.lookup loc vars

let get_maybe_obj ((objs, _, _) as store) loc =
  if Store.mem loc objs
  then Some (get_obj store loc)
  else None

let get_maybe_var ((_, vars, _) as store) loc =
  if Store.mem loc vars
  then Some (get_var store loc)
  else None

let set_obj ((objs, vars, ann) : store) (loc : Store.loc) new_obj =
  (Store.update loc new_obj objs), vars, ann
let set_var (objs, vars, ann) loc new_val =
  (objs, Store.update loc new_val vars, ann)

let add_obj (objs, vars, ann) new_obj =
  let new_loc, objs' = Store.alloc new_obj objs in
  new_loc, (objs', vars, ann)
let add_var (objs, vars, ann) new_val =
  let new_loc, vars' = Store.alloc new_val vars in
  new_loc, (objs, vars', ann)

exception Break of exp list * label * value * store
exception Throw of exp list * value * store
exception PrimErr of exp list * value
exception Snapshot of exp list * value * env list * store
exception AnnErr of exp list * value * store

let pretty_value v = match v with
  | Num (d, annotation) -> 
      string_of_float d ^ "<!" ^ string_of_ann annotation ^ "!>"
  | String (s, annotation) -> 
      "\"" ^ s ^ "\"" ^ "<!" ^ string_of_ann annotation ^ "!>"
  | True annotation -> "true" ^ "<!" ^ string_of_ann annotation ^ "!>"
  | False annotation -> "false" ^ "<!" ^ string_of_ann annotation ^ "!>"
  | Undefined -> "undefined"
  | Null -> "null"
  | ObjLoc _ -> "object"
  (*TODO: These never return*)
  | Closure _ -> "function"
  | ClosureEnf _ -> "function"
