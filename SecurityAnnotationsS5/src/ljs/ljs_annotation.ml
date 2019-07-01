open Prelude

type atomic_annotation =
  | Top
  | Atom of string

type annotation = atomic_annotation list

(* Processing Strings of Annotations *)
let strToAnn ann = match ann with
  | "Top" -> Top
  | "" -> Top
  | str -> Atom str

let parseAnn ann = 
  let delim = Str.regexp "*" in
  let tmp = Str.split delim ann in
  let tmp2 = List.map String.trim tmp in
  List.map strToAnn tmp2

(* Constructs hierarchy map. *)
module Hierarchy = 
  Map.Make(struct type t = atomic_annotation let compare = compare end)

let string_of_atomic_ann atomic_ann = match atomic_ann with
  | Top -> "Top"
  | Atom s -> s

(* Note we print subannotations if both have been added.*)
let rec string_of_ann ann = match ann with
  | [] -> "Top"
  | head::[] -> string_of_atomic_ann head
  | head::[Top] -> string_of_atomic_ann head
  | Top::body -> string_of_ann body
  | head::body -> (string_of_atomic_ann head) ^ " * " ^ (string_of_ann body)

let pretty_ann ann = 
  "<!" ^ string_of_ann ann ^ "!>"

(* 
 * Removes duplicate annotations from the list
 * via standard filtering with a hashmap
 *)
let remove_dup_ann ann = 
  (* Ensures all values have Top annotation *)
  let ann = ann@[Top] in 
  let seen = Hashtbl.create (List.length ann) in
  List.filter (function atom ->
                        let tmp = not (Hashtbl.mem seen atom) in
                        Hashtbl.replace seen atom 1;
                        tmp) ann

(*Manipulating the annotation hierarchy in the store*)
let add_TL_hierarchy (atom : atomic_annotation) (objs, vars, hier)  =
  let new_hier = match (Hierarchy.mem atom hier) with
    | true -> hier
   (* Add in the new annotation.*)
    | false -> Hierarchy.add atom [] hier in
  (objs, vars, new_hier)

let get_global_ann (_, _, ann) = ann
let set_global_ann (ann : annotation) store = 
  List.fold_right add_TL_hierarchy ann store

(*Adding new subtypes to hierarchy*)
let update_sub_of_super super sub (objs, vars, hier) = 
  let old_sub = Hierarchy.find super hier in
  let new_sub = old_sub@sub in
  (*Add all the sub annotations to the hierarchy at the key level*)
  let (_, _, tmp_hier) = set_global_ann sub (objs, vars, hier) in
  (* Annoyingly update is not available in Ocaml 4.02,
   * so we have to remove the key, then add it again with the new subs*)
  let new_hier = 
    Hierarchy.add super new_sub (Hierarchy.remove super tmp_hier) in
  (* Prints hierarchy for error-checking code:
  let print_anns_subs a subs = print_string 
    ((string_of_atomic_ann a) ^ " has subs " ^ (string_of_ann subs) ^ "\n") in
  Hierarchy.iter print_anns_subs new_hier;
  *)
  (objs, vars, new_hier)

(*
 * Check the annotation being added is indeed valid in this program
 * i.e. that it has already been declared in the hierarchy.
 *)
let check_ann_valid ann store = 
  List.for_all (function atom -> 
      match atom with 
       | Top -> true
      (* We added every annotation as a key regardless of where it is in the 
       * hierarchy so this check is easy.*)
       | x -> Hierarchy.mem x (get_global_ann store)) ann

(*
 * Functions for checking subannotations
 *)

(* a1 <: a2 ? where both are atomics*)
let rec is_sub_atomic_atomic a1 a2 (objs, vars, hier) = 
 (*
  * 1. Distinguish Top cases.
  * 2. Check if a1 is an immediate subannotation of a2
  * 3. If not, check recursively on each immediate subannotation of a2.
  *)
  match (a1, a2) with
   | (_, Top) -> true
   | (Top, _) -> false
   | (a1, a2) ->
      let a2Subs = Hierarchy.find a2 hier in
      if (a1 = a2 || List.mem a1 a2Subs) then true else
      List.exists (function x -> 
        is_sub_atomic_atomic a1 x (objs, vars, hier)) a2Subs

(* a1 <: s ? where s is a product*)
(*TODO:SEEMS WRONG - THIS IS SOMETHING ELSE? TRACK IT AND CUT DOWN.*)
let is_sub_atomic_prod a s store = 
  if (List.mem a s) then true else
    List.exists (function x -> is_sub_atomic_atomic a x store) s

(* s <: a ? where s is a product*)
let is_sub_prod_atomic s a store = 
  if (List.mem a s) then true else
    List.exists (function x -> is_sub_atomic_atomic x a store) s

(* s1 <: s2 ? where both are products*)
let is_sub s1 s2 store = 
  (* Logging for bug finding.
  print_string
    ("We ask: " ^ string_of_ann s1 ^ " <: " ^ string_of_ann s2 ^ "?\n");*)
  List.for_all (function x -> is_sub_prod_atomic s1 x store) s2

(*
 * 
 * s1 is the annotation attached to the argument, which needs to type check
 * s2 is the enforcer annotation we are checking against
 *
 * This function checks that the annotation on the argument is a subannotation 
 * (i.e. is more specific) than the one which is being checked for
 *)
let compare_annotation s1 s2 store =
  (* Top-based filtering just, else everything will typecheck.*)
  let a1 = match remove_dup_ann s1 with
   | [Top] -> [Top]
   | s -> List.filter (function x -> not (x == Top)) s in
  match remove_dup_ann s2 with
   | [Top] -> true
   | s -> let a2 = List.filter (function x -> not (x == Top)) s in
          is_sub a1 a2 store

(*AS and DROP logics*)

(*Helper function grabs immediate parent of an atomic annotation*)
let parent a (_, _, hier) = 
  match a with
   | Top -> Top
   | a -> 
      let p k v = List.mem a v in
      let tmp = Hierarchy.filter p hier in
      if (Hierarchy.is_empty tmp) then Top else
        (*This is guaranteed to be a singleton by definition of our maps.*)
        let (res, _) = Hierarchy.choose tmp in
        res

(* The cut operator on elements of the lattices *)
let cut a1 a2 store = 
  match (a1, a2) with
   | (_, [Top]) -> [Top]
   | ([Top], _) -> [Top]
   | (a1, a2) -> 
      (*In the initial case there is nothing to cut *)
      if not (is_sub a1 a2 store) then a1 else
        (* Let s1...sn be a1, r1...rm be a2
         * for each ri such that sj <: ri, add parent ri
         * if no ri satisfy, then add sj
         *)
        let f s tmp = 
          if (is_sub_atomic_prod s a2 store) 
            then 
              let g x = is_sub_atomic_atomic s x store in
              let tmp2 = List.filter g a2 in
              tmp@(List.map (function x -> parent x store) tmp2)
            else tmp@[s]
          in
        List.fold_right f a1 [Top]

(*The as operator, which is super-simple.*)
let add_ann a1 a2 = 
  List.append a1 a2
