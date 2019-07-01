open Prelude
open Ljs_syntax
open Ljs_values
open Ljs_pretty_value
open Ljs_annotation

let undef = Undefined
let null = Null
let str (s, ann) = String (s, ann)
let num (f, ann) = Num (f, ann)

let bool b = match b with
  | true -> True [Top]
  | false -> False [Top]

let to_int v = match v with
  | Num (x, _) -> int_of_float x
  | _ -> raise (PrimErr ([], str (("expected number, got " ^ pretty_value v), [Top])))

let typeof store v = str begin match v with
  | Undefined -> ("undefined", [Top])
  | Null -> ("null", [Top])
  | String (_, _) -> ("string", [Top])
  | Num (_, _) -> ("number", [Top])
  | True _
  | False _ -> ("boolean", [Top])
  | ObjLoc loc -> begin match get_obj store loc with
      | ({ code = Some cexp }, _, _) -> ("function", [Top])
      | _ -> ("object", [Top])
  end
  | Closure _ -> ("function", [Top]) (*raise (PrimErr ([], str "typeof got lambda"))*)
  | ClosureEnf _ -> ("function", [Top]) (*raise (PrimErr ([], str "typeof got lambda"))*)
end

let is_closure store v = match v with
  | Closure _ -> bool true
  | _ -> bool false

let is_annotatable store v = match v with
  | String (_, _)
  | Num (_, _)
  | True _ | False _ -> True [Top]
  | ObjLoc loc -> begin match get_obj store loc with
      | ({ code = Some cexp }, _, _) -> False [Top]
      | _ -> True [Top]
  end
  | _ -> False [Top]

let is_primitive store v = match v with
  | Undefined 
  | Null
  | String (_, _)
  | Num (_, _)
  | True _ | False _ -> True [Top]
  | _ -> False [Top]

let float_str n = 
  if not (n <= n || n >= n) then "NaN"
  else
    if n == infinity then "Infinity"
    else if n == neg_infinity then "-Infinity"
    else
      if float_of_int (int_of_float n) = n
      then string_of_int (int_of_float n) 
      else string_of_float n

(*
 * We shouldn't convert the annotations to strings here - the printer 
 * does this for program printing purposes.
 * However it is necessary to add the Top annotation to everything, except 
 * strings themselves, which are unaffected by the operation.
 *)
let prim_to_str store v = str begin match v with
  | Undefined -> ("undefined", [Top])
  | Null -> ("null", [Top])
  | String (s, ann) -> (s, ann)
  | Num (n, annotation) -> let res = 
    (let fs = float_str n in let fslen = String.length fs in
    if String.get fs (fslen - 1) = '.' then String.sub fs 0 (fslen - 1) else
      (* This is because we don't want leading zeroes in the "-e" part.
       * For example, OCaml says 1.2345e-07, but ES5 wants 1.2345e-7 *)
      if String.contains fs '-'
        then let indx = String.index fs '-' in 
          let prefix = String.sub fs 0 (indx + 1) in
          let suffix = String.sub fs (indx + 1) (fslen - (String.length prefix)) in
          let slen = String.length suffix in
          let fixed = if slen > 1 && (String.get suffix 0 = '0') 
            then String.sub suffix 1 (slen - 1)
            else suffix in
          prefix ^ fixed 
        else fs) in
    (res, [Top]) (*^ "<" ^ string_of_ann annotation ^ ">"*)
  | True annotation -> ("true", [Top]) 
    (*^ "<" ^ string_of_ann annotation ^ ">"*)
  | False annotation -> ("false", [Top])
    (*^ "<" ^ string_of_ann annotation ^ ">"*)
  | _ -> raise (PrimErr ([], str ("prim_to_str", [Top])))
end

let strlen store s = match s with
  | String (s, _) -> Num ((float_of_int (String.length s)), [Top])
  | _ -> raise (PrimErr ([], str ("strlen", [Top])))

(* Section 9.3, excluding objects *)
let prim_to_num store v = num begin match v with
  | Undefined -> (nan, [Top]) 
  | Null -> (0.0, [Top])
  | True _ -> (1.0, [Top])
  | False _ -> (0.0, [Top])
  | Num (x, ann) -> (x, ann)
  | String ("", _) -> (0.0, [Top])
  | String (s, _) -> begin try (float_of_string s, [Top])
    with Failure _ -> (nan, [Top]) end
  | _ -> raise (PrimErr ([], str ("prim_to_num", [Top])))
end
  
let prim_to_bool store v = bool begin match v with
  | True _ -> true
  | False _ -> false
  | Undefined -> false
  | Null -> false
  | Num (x, _) -> not (x == nan || x = 0.0 || x = -0.0)
  | String (s, _) -> not (String.length s = 0)
  | _ -> true
end

let print store v = match v with
  | String (s, ann) -> 
      let p = s ^ "<!" ^ string_of_ann ann ^ "!>" in
      printf "%s\n%!" p; Undefined
  | Num (n, ann) -> 
      let p = string_of_float n ^ "<!" ^ string_of_ann ann ^ "!>" in
      printf "%S\n" p; Undefined
  | _ -> failwith ("[interp] Print received non-string: " ^ pretty_value v)

let pretty store v =
  let x = string_of_value v store in
  printf "%s\n%!" x; Undefined

let is_extensible store obj = match obj with
  | ObjLoc loc -> begin match get_obj store loc with
      | ({ extensible = true; }, _, _) -> True [Top]
      | _ -> False [Top]
  end
  | _ -> raise (PrimErr ([], str("is-extensible", [Top])))

(* Implement this here because there's no need to expose the class
   property outside of the delta function *)
let object_to_string store obj = match obj with
  | ObjLoc loc -> begin match get_obj store loc with
      | ({ klass = s }, _, _) -> str (("[object " ^ s ^ "]"), [Top])
  end
  | _ -> raise 
      (PrimErr ([], str("object-to-string, wasn't given object", [Top])))	

let is_array store obj = match obj with
  | ObjLoc loc -> begin match get_obj store loc with
      | ({ klass = "Array"; }, _, _) -> True [Top]
      | _ -> False [Top]
    end
  | _ -> raise (PrimErr ([], str("is-array", [Top])))	


let to_int32 store v = match v with
  | Num (d, ann) -> Num ((float_of_int (int_of_float d)), ann)
  | _ -> raise (PrimErr ([], str("to-int", [Top])))

let nnot store e = match e with
  | Undefined -> True [Top]
  | Null -> True [Top]
  | True _ -> False [Top]
  | False _ -> True [Top]
  | Num (d, _) -> if (d = 0.) || (d <> d) then True [Top] else False [Top]
  | String (s, _) -> if s = "" then True [Top] else False [Top]
  | ObjLoc _ -> False [Top]
  | Closure _ -> False [Top]
  | ClosureEnf _ -> False [Top]

let void store v = Undefined

let floor' store = function Num (d, _) -> num ((floor d), [Top]) | _ -> raise (PrimErr ([], str("floor", [Top])))

let ceil' store= function Num (d, _) -> num ((ceil d), [Top]) | _ -> raise (PrimErr ([], str("ceil", [Top])))

let absolute store = function Num (d, _) -> num ((abs_float d), [Top]) | _ -> raise (PrimErr ([], str("abs", [Top])))

let log' store = function Num (d, _) -> num ((log d ), [Top]) | _ -> raise (PrimErr ([], str("log", [Top])))

let ascii_ntoc store n = match n with
  | Num (d, _) -> str ((String.make 1 (Char.chr (int_of_float d))), [Top])
  | _ -> raise (PrimErr ([], str("ascii_ntoc", [Top]))) 
let ascii_cton store c = match c with
  | String (s, _) -> Num ((float_of_int (Char.code (String.get s 0))), [Top])
  | _ -> raise (PrimErr ([], str("ascii_cton", [Top])))

let to_lower store = function
  | String (s, _) -> String (String.lowercase s, [Top])
  | _ -> raise (PrimErr ([], str("to_lower", [Top])))

let to_upper store = function
  | String (s, _) -> String (String.uppercase s, [Top])
  | _ -> raise (PrimErr ([], str("to_lower", [Top])))

let bnot store = function
  | Num (d, _) -> Num ((float_of_int (lnot (int_of_float d))), [Top])
  | _ -> raise (PrimErr ([], str("bnot", [Top])))

let sine store = function
  | Num (d, _) -> Num ((sin d), [Top])
  | _ -> raise (PrimErr ([], str("sin", [Top])))

let numstr store = function
  | String ("", _) -> Num (0., [Top])
  | String (s, _) -> Num ((try float_of_string s with Failure _ -> nan), [Top])
  | _ -> raise (PrimErr ([], str("numstr", [Top])))

let current_utc store = function
  | _ -> Num ((Unix.gettimeofday ()), [Top])

let op1 store op = match op with
  | "typeof" -> typeof store
  | "closure?" -> is_closure store
  | "primitive?" -> is_primitive store
  | "prim->str" -> prim_to_str store
  | "prim->num" -> prim_to_num store
  | "prim->bool" -> prim_to_bool store
  | "print" -> print store
  | "pretty" -> pretty store 
  | "object-to-string" -> object_to_string store
  | "strlen" -> strlen store
  | "is-array" -> is_array store
  | "to-int32" -> to_int32 store
  | "!" -> nnot store
  | "void" -> void store
  | "floor" -> floor' store
  | "ceil" -> ceil' store
  | "abs" -> absolute store
  | "log" -> log' store
  | "ascii_ntoc" -> ascii_ntoc store
  | "ascii_cton" -> ascii_cton store
  | "to-lower" -> to_lower store
  | "to-upper" -> to_upper store
  | "~" -> bnot store
  | "sin" -> sine store
  | "numstr->num" -> numstr store
  | "current-utc-millis" -> current_utc store
  | _ -> raise (PrimErr ([], String(("no implementation of unary operator: " ^ op), [Top])))

let arith store s i_op f_op v1 v2 = match v1, v2 with
  | Num (x, _), Num (y, _) -> Num ((f_op x y), [Top])
  | v1, v2 -> raise (PrimErr ([], str(("arithmetic operator: " ^ s ^ " got non-numbers: " ^
                                 (pretty_value v1) ^ ", " ^ (pretty_value v2) ^
                                   "perhaps something wasn't desugared fully?"),[Top])))

let arith_sum store = arith store "+" (+) (+.)

let arith_sub store = arith store "-" (-) (-.)

(* OCaml syntax failure! Operator section syntax lexes as a comment. *)
let arith_mul store = arith store "*" (fun m n -> m * n) (fun x y -> x *. y)

let arith_div store x y = try arith store "/" (/) (/.) x y
with Division_by_zero -> Num (infinity, [Top])

let arith_mod store x y = try arith store "mod" (mod) mod_float x y
with Division_by_zero -> Num (nan, [Top])

let arith_lt store x y = bool (x < y)

let arith_le store x y = bool (x <= y)

let arith_gt store x y = bool (x > y)

let arith_ge store x y = bool (x >= y)

let bitwise_and store v1 v2 = 
  Num ((float_of_int ((to_int v1) land (to_int v2))), [Top])

let bitwise_or store v1 v2 = 
  Num ((float_of_int ((to_int v1) lor (to_int v2))), [Top])

let bitwise_xor store v1 v2 =
  Num ((float_of_int ((to_int v1) lxor (to_int v2))), [Top])

let bitwise_shiftl store v1 v2 = 
  Num ((float_of_int ((to_int v1) lsl (to_int v2))), [Top])

let bitwise_zfshiftr store v1 v2 = 
  Num ((float_of_int ((to_int v1) lsr (to_int v2))), [Top])

let bitwise_shiftr store v1 v2 = 
  Num ((float_of_int ((to_int v1) asr (to_int v2))), [Top])

let string_plus store v1 v2 = match v1, v2 with
  | String (s1, _), String (s2, _) ->
      String ((s1 ^ s2), [Top])
  | _ -> raise (PrimErr ([], str("string concatenation", [Top])))

let string_lessthan store v1 v2 = match v1, v2 with
  | String (s1, _), String (s2, _) -> bool (s1 < s2)
  | _ -> raise (PrimErr ([], str("string less than", [Top])))

let stx_eq store v1 v2 = bool begin match v1, v2 with
  | Num (x1, _), Num (x2, _) -> x1 = x2
  | String (x1, _), String (x2, _) -> x1 = x2
  | Undefined, Undefined -> true
  | Null, Null -> true
  | True _, True _ -> true
  | False _, False _ -> true
  | _ -> v1 == v2 (* otherwise, pointer equality *)
end

(* Algorithm 11.9.3, steps 1 through 19. Steps 20 and 21 are desugared to
   access the heap. *)
let abs_eq store v1 v2 = bool begin
  if v1 = v2 then (* works correctly on floating point values *)
    true
  else match v1, v2 with
    | Null, Undefined
    | Undefined, Null -> true
    | String (s, _), Num (x, _)
    | Num (x, _), String (s, _) ->
          (try x = float_of_string s with Failure _ -> false)
    | Num (x, _), True _ | True _, Num (x, _) -> x = 1.0
    | Num (x, _), False _ | False _, Num (x, _) -> x = 0.0
    | _ -> false
(* TODO: are these all the cases? *)
end

(* Algorithm 9.12, the SameValue algorithm.
   This gives "nan = nan" and "+0 != -0". *)
let same_value store v1 v2 = bool begin
  match v1, v2 with
  | Num (x, _), Num (y, _) ->
    if x = 0. && y = 0.
    then 1. /. x = 1. /. y
    else compare x y = 0
  | _ -> compare v1 v2 = 0
end

let rec has_property store obj field = match obj, field with
  | ObjLoc loc, String (s, _) -> begin match get_obj store loc, s with
      ({ proto = pvalue; }, props, _), s ->
        if (IdMap.mem s props) then bool true
        else has_property store pvalue field
  end
  | _ -> bool false

let has_own_property store obj field = match obj, field with
  | ObjLoc loc, String (s, _) -> 
      let (attrs, props, ann) = get_obj store loc in
        bool (IdMap.mem s props)
  | ObjLoc loc, _ -> raise (PrimErr ([], str("has-own-property: field not a string", [Top])))
  | _, String (s, _) -> raise (PrimErr ([], str(("has-own-property: obj not an object for field " ^ s), [Top])))
  | _ -> raise (PrimErr ([], str("has-own-property: neither an object nor a string", [Top])))

let base store n r = 
  let rec get_digits n l = match n with
    | 97 -> 'a' :: l
    | i -> get_digits (n - 1) ((Char.chr i) :: l) in
  let digits = 
    ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] @ (get_digits 122 []) in
  let rec get_num_digits num so_far =
    if (r ** so_far) > num then so_far -. 1.0
    else get_num_digits num (so_far +. 1.0) in
  let rec convert b result len = 
    let lp = r ** len in
    let index = floor (b /. lp) in
    let digit = String.make 1 (List.nth digits (int_of_float index)) in
    if len = 0.0 then result ^ digit
    else convert (b -. (index *. lp)) (result ^ digit)  (len -. 1.0) in
  (*let rec shift frac n = if n = 0 then frac else shift (frac *. 10.0) (n - 1) in*)
  let (f, integ) = modf n in
  (* TODO(joe): shifted is unused *)
  (* let shifted = shift f ((String.length (string_of_float f)) - 2) in *)
  if (f = 0.0) then
    let d = get_num_digits n 0.0 in
    convert n "" d
  else
    (* TODO: implement *)
    "non-base-10 with fractional parts NYI"

let get_base store n r = match n, r with
  | Num (x, _), Num (y, _) -> 
    let result = base store (abs_float x) (abs_float y) in
    str((if x < 0.0 then "-" ^ result else result), [Top])
  | _ -> raise (PrimErr ([], str("base got non-numbers", [Top])))

let char_at store a b  = match a, b with
  | String (s, _), Num (n, _) ->
    String ((String.make 1 (String.get s (int_of_float n))), [Top])
  | _ -> raise 
      (PrimErr ([], str("char_at didn't get a string and a number", [Top])))

let locale_compare store a b = match a, b with
  | String (r, _), String (s, _) ->
    Num ((float_of_int (String.compare r s)), [Top])
  | _ -> raise 
      (PrimErr ([], str("locale_compare didn't get 2 strings", [Top])))

let pow store a b = match a, b with
  | Num (base, _), Num (exp, _) -> Num ((base ** exp), [Top])
  | _ -> raise (PrimErr ([], str("pow didn't get 2 numbers", [Top])))

let to_fixed store a b = match a, b with
  | Num (x, _), Num (f, _) -> 
    let s = string_of_float x
    and fint = int_of_float f in
    if fint = 0 
      then String ((string_of_int (int_of_float x)), [Top]) 
      else let dot_index = String.index s '.'
      and len = String.length s in
      let prefix_chars = dot_index in
      let decimal_chars = len - (prefix_chars + 1) in
      if decimal_chars = fint then String (s, [Top])
      else if decimal_chars > fint
        then let fixed_s = String.sub s 0 (fint - prefix_chars) in
        String ((fixed_s), [Top])
      else let suffix = String.make (fint - decimal_chars) '0' in
        String ((s ^ suffix), [Top])
  | _ -> raise (PrimErr ([], str ("to-fixed didn't get 2 numbers", [Top])))

let rec is_accessor store a b = match a, b with
  | ObjLoc loc, String (s, _) ->
    let (attrs, props, ann) = get_obj store loc in
    if IdMap.mem s props
    then let prop = IdMap.find s props in
      match prop with
        | Data _ -> False [Top]
        | Accessor _ -> True [Top]
    else let pr = match attrs with { proto = p } -> p in
      is_accessor store pr b
  | Null, String (s, _) -> 
      raise (PrimErr ([], str("isAccessor topped out", [Top])))
  | _ -> raise (PrimErr ([], str("isAccessor", [Top])))

let op2 store op = match op with
  | "+" -> arith_sum store
  | "-" -> arith_sub store
  | "/" -> arith_div store
  | "*" -> arith_mul store
  | "%" -> arith_mod store
  | "&" -> bitwise_and store
  | "|" -> bitwise_or store
  | "^" -> bitwise_xor store
  | "<<" -> bitwise_shiftl store
  | ">>" -> bitwise_shiftr store
  | ">>>" -> bitwise_zfshiftr store
  | "<" -> arith_lt store
  | "<=" -> arith_le store
  | ">" -> arith_gt store
  | ">=" -> arith_ge store
  | "stx=" -> stx_eq store
  | "abs=" -> abs_eq store
  | "sameValue" -> same_value store
  | "hasProperty" -> has_property store
  | "hasOwnProperty" -> has_own_property store
  | "string+" -> string_plus store
  | "string<" -> string_lessthan store
  | "base" -> get_base store
  | "char-at" -> char_at store
  | "locale-compare" -> locale_compare store
  | "pow" -> pow store
  | "to-fixed" -> to_fixed store
  | "isAccessor" -> is_accessor store
  | _ -> raise (PrimErr ([], String(("no implementation of binary operator: " ^ op), [Top])))
