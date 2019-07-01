open Prelude
open Ljs_cps_values

let newLabel = Ljs_cps.Label.newLabel
let str s = String (Pos.dummy, newLabel(), s)
let num f = Num (Pos.dummy, newLabel(), f)

exception CpsThrow of string

let bool b = match b with
  | true -> True (Pos.dummy, newLabel())
  | false -> False (Pos.dummy, newLabel())
let unbool b = match b with
  | True _ -> true
  | False _ -> false
  | _ -> failwith ("tried to unbool a non-bool")

let to_int v = match v with
  | Num (_, _, x) -> int_of_float x
  | _ -> raise (CpsThrow ( ("expected number in es5_cps_delta.to_int: " ^ (pretty_bind v))))

let typeof v _ = str begin match v with
  | Undefined _ -> "undefined"
  | Null _-> "null"
  | String _ -> "string"
  | Num _ -> "number"
  | True _
  | False _-> "boolean"
  | Object (_, _, { code = Some _ }, _) -> "function"
  | Object _ -> "object"
  | Closure _ -> raise (CpsThrow ( "typeof got lambda"))
  | VarCell _ -> failwith "[cps-interp] Can't get typeof VarCell!"
end
  
let is_primitive v _ = match v with
  | Undefined _
  | Null _
  | String _
  | Num _
  | True _ | False _ -> True (Pos.dummy, newLabel())
  | _ -> False (Pos.dummy, newLabel())

let float_str n _ = 
  if n == nan then "NaN"
  else
    if n == infinity then "Infinity"
    else if n == neg_infinity then "-Infinity"
    else
      if float_of_int (int_of_float n) = n
      then string_of_int (int_of_float n) 
      else string_of_float n

let prim_to_str v store = str begin match v with
  | Undefined _ -> "undefined"
  | Null _ -> "null"
  | String (_, _, s) -> s
  | Num (_, _, n) -> let fs = float_str n store in let fslen = String.length fs in
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
        else fs
  | True _ -> "true"
  | False _ -> "false"
  | _ -> raise (CpsThrow ( "prim_to_num"))
end

let strlen s _ = match s with
  | String (_, _, s) -> Num (Pos.dummy, newLabel(), (float_of_int (String.length s)))
  | _ -> raise (CpsThrow ( "strlen"))

(* Section 9.3, excluding objects *)
let prim_to_num v _ = num begin match v with
  | Undefined _ -> nan 
  | Null _ -> 0.0
  | True _ -> 1.0
  | False _-> 0.0
  | Num (_, _, x) -> x
  | String (_, _, "") -> 0.0
  | String (_, _, s) -> begin try float_of_string s
    with Failure _ -> nan end
  | _ -> raise (CpsThrow ( "prim_to_num"))
end
  
let prim_to_bool v _ = bool begin match v with
  | True _ -> true
  | False _ -> false
  | Undefined _ -> false
  | Null _ -> false
  | Num (_, _, x) -> not (x == nan || x = 0.0 || x = -0.0)
  | String (_, _, s) -> not (String.length s = 0)
  | _ -> true
end

let print v _ = match v with
  | String (_, _, s) -> 
      printf "%S\n%!" s; Undefined (Pos.dummy, newLabel())
  | Num (_, _, n) -> let s = string_of_float n in printf "%S\n" s; Undefined (Pos.dummy, newLabel())
  | _ -> failwith ("[cps-interp] Print received non-string: " ^ (pretty_bind v))

(* Implement this here because there's no need to expose the class
   property outside of the delta function *)
let object_to_string obj store = match obj with
  | VarCell(_, _, a) -> (match (Store.find a store) with
    | Object(_, _, {klass=s},_) -> str ("[object " ^ s ^ "]")
    | _ -> raise (CpsThrow ( "object-to-string, wasn't given object: " ^ (pretty_bind obj))))
  | _ -> raise (CpsThrow ( "object-to-string, wasn't given VarCell: " ^ (pretty_bind obj)))

let is_array obj store = match obj with
  | VarCell(_, _, a) -> (match (Store.find a store) with
    | Object(_, _, {klass="Array"},_) -> bool true
    | Object _ -> bool false
    | _ -> raise (CpsThrow ( "is-array: " ^ (pretty_bind obj))))
  | _ -> raise (CpsThrow ( "is-array: " ^ (pretty_bind obj)))


let to_int32 v _ = match v with
  | Num (_,_,d) -> Num(Pos.dummy, newLabel(), (float_of_int (int_of_float d)))
  | _ -> raise (CpsThrow ( "to-int: " ^ (pretty_bind v)))

let nnot e _ = match e with
  | Undefined _ -> bool true
  | Null _ -> bool true
  | True _ -> bool false
  | False _ -> bool true
  | Num (_, _, d) -> if (d = 0.) || (d <> d) then bool true else bool false
  | String (_, _, s) -> if s = "" then bool true else bool false
  | Object _ -> bool false
  | Closure _ -> bool false
  | VarCell _ -> failwith "[cps-interp] Can't get nnot VarCell!"

let void v _ = Undefined (Pos.dummy, newLabel())

let floor' n _ = match n with Num (_, _, d) -> num (floor d) | _ -> raise (CpsThrow ( "floor"))

let ceil' n _ = match n with Num (_, _, d) -> num (ceil d) | _ -> raise (CpsThrow ( "ceil"))

let absolute n _ = match n with Num (_, _, d) -> num (abs_float d) | _ -> raise (CpsThrow ( "abs"))

let log' n _ = match n with Num (_, _, d) -> num (log d ) | _ -> raise (CpsThrow ( "log"))

let ascii_ntoc n _ = match n with
  | Num (_, _, d) -> str (String.make 1 (Char.chr (int_of_float d)))
  | _ -> raise (CpsThrow ( "ascii_ntoc"))

let ascii_cton c _ = match c with
  | String (_, _, s) -> num (float_of_int (Char.code (String.get s 0)))
  | _ -> raise (CpsThrow ( "ascii_cton"))

let to_lower s _ = match s with
  | String (_, _, s) -> str (String.lowercase s)
  | _ -> raise (CpsThrow ( "to_lower"))

let to_upper s _ = match s with
  | String (_, _, s) -> str (String.uppercase s)
  | _ -> raise (CpsThrow ( "to_lower"))

let bnot b _ = match b with
  | Num (_, _, d) -> num (float_of_int (lnot (int_of_float d)))
  | _ -> raise (CpsThrow ( "bnot"))

let sine n _ = match n with
  | Num (_, _, d) -> num (sin d)
  | _ -> raise (CpsThrow ( "sin"))

let numstr s _ = match s with
  | String (_, _, s) -> num (try float_of_string s with Failure _ -> nan)
  | _ -> raise (CpsThrow ( "numstr"))

let op1 op : bind_value -> bind_value Store.t -> bind_value = match op with
  | "typeof" -> typeof
  | "primitive?" -> is_primitive
  | "prim->str" -> prim_to_str
  | "prim->num" -> prim_to_num
  | "prim->bool" -> prim_to_bool
  | "print" -> print
  | "object-to-string" -> object_to_string
  | "strlen" -> strlen
  | "is-array" -> is_array
  | "to-int32" -> to_int32
  | "!" -> nnot
  | "void" -> void
  | "floor" -> floor'
  | "ceil" -> ceil'
  | "abs" -> absolute
  | "log" -> log'
  | "ascii_ntoc" -> ascii_ntoc
  | "ascii_cton" -> ascii_cton
  | "to-lower" -> to_lower
  | "to-upper" -> to_upper
  | "~" -> bnot
  | "sin" -> sine
  | "numstr->num" -> numstr
  | _ -> failwith ("no implementation of unary operator: " ^ op)

let arith s f_op v1 v2 _ = match v1, v2 with
  | Num (_, _, x), Num (_, _, y) -> num (f_op x y)
  | v1, v2 -> raise (CpsThrow ( ("arithmetic operator: " ^ s ^ " got non-numbers, " ^
                                   "perhaps something wasn't desugared fully?")))

let compare s f_op v1 v2 _ = match v1, v2 with
  | Num (_, _, x), Num (_, _, y) -> bool (f_op x y)
  | v1, v2 -> raise (CpsThrow ( ("compare operator: " ^ s ^ " got non-numbers, " ^
                                   "perhaps something wasn't desugared fully?")))

let arith_sum = arith "+" (+.)

let arith_sub = arith "-" (-.)

(* OCaml syntax failure! Operator section syntax lexes as a comment. *)
let arith_mul = arith "*" (fun x y -> x *. y)

let arith_div x y env = try arith "/" (/.) x y env
with Division_by_zero -> num infinity

let arith_mod x y env = try arith "mod" mod_float x y env
with Division_by_zero -> num nan

let arith_lt = compare "<" (<) 

let arith_le = compare "<=" (<=) 

let arith_gt = compare ">" (>) 

let arith_ge = compare ">=" (>=) 

let bitwise_and v1 v2 _ = num (float_of_int ((to_int v1) land (to_int v2)))

let bitwise_or v1 v2 _ = num (float_of_int ((to_int v1) lor (to_int v2)))

let bitwise_xor v1 v2 _ = num (float_of_int ((to_int v1) lxor (to_int v2)))

let bitwise_shiftl v1 v2 _ = num (float_of_int ((to_int v1) lsl (to_int v2)))

let bitwise_zfshiftr v1 v2 _ = num (float_of_int ((to_int v1) lsr (to_int v2)))

let bitwise_shiftr v1 v2 _ = num (float_of_int ((to_int v1) asr (to_int v2)))

let string_plus v1 v2 _ = match v1, v2 with
  | String (_, _, s1), String (_, _, s2) ->
      str (s1 ^ s2)
  | _ -> raise (CpsThrow ( "string concatenation"))

let string_lessthan v1 v2 _ = match v1, v2 with
  | String (_, _, s1), String (_, _, s2) -> bool (s1 < s2)
  | _ -> raise (CpsThrow ( "string less than"))

let stx_eq v1 v2 _ = bool begin match v1, v2 with
  | Num (_, _, x1), Num (_, _, x2) -> x1 = x2
  | String (_, _, x1), String (_, _, x2) -> x1 = x2
  | Undefined _, Undefined _ -> true
  | Null _, Null _ -> true
  | True _, True _ -> true
  | False _, False _ -> true
  | _ -> v1 == v2 (* otherwise, pointer equality *)
end

(* Algorithm 11.9.3, steps 1 through 19. Steps 20 and 21 are desugared to
   access the heap. *)
let abs_eq v1 v2 _ = bool begin
  if v1 = v2 then (* works correctly on floating point values *)
    true
  else match v1, v2 with
    | Null _, Undefined _
    | Undefined _, Null _ -> true
    | String (_, _, s), Num (_, _, x)
    | Num (_, _, x), String (_, _, s) ->
          (try x = float_of_string s with Failure _ -> false)
    | Num (_, _, x), True _ | True _, Num (_, _, x) -> x = 1.0
    | Num (_, _, x), False _ | False _, Num (_, _, x) -> x = 0.0
    | _ -> false
(* TODO: are these all the cases? *)
end

let rec has_property obj field store = match obj with
  | VarCell (_, _, a) -> (match (Store.find a store), field with
    | Object(_, _, {proto=p}, props), String (_, _, s) -> 
      if List.mem_assoc s props 
      then bool true
      else (match p with None -> bool false | Some proto -> has_property proto field store)
    | _ -> bool false)
  | _ -> bool false

let has_own_property obj field store = match obj with
  | VarCell (_, _, a) -> (match (Store.find a store), field with
    | Object(_, _, {proto=p}, props), String (_, _, s) -> bool (List.mem_assoc s props)
    | Object _, _ -> raise (CpsThrow ( "has-own-property: field not a string"))
    | _, String (_, _, s) -> raise (CpsThrow ( ("has-own-property: obj not an object for field " ^ s)))
    | _ -> raise (CpsThrow ( "has-own-property: neither an object nor a string")))
  | _ -> raise (CpsThrow ("[cps-interp] has-own-property didn't get a VarCell"))

let base n r = 
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
  let rec shift frac n = if n = 0 then frac else shift (frac *. 10.0) (n - 1) in
  let (f, integ) = modf n in
  (* TODO(joe): shifted is unused *)
  (* let shifted = shift f ((String.length (string_of_float f)) - 2) in *)
  if (f = 0.0) then
    let d = get_num_digits n 0.0 in
    convert n "" d
  else
    (* TODO: implement *)
    "non-base-10 with fractional parts NYI"

let get_base n r _ = match n, r with
  | Num (_, _, x), Num (_, _, y) -> 
    let result = base (abs_float x) (abs_float y) in
    str (if x < 0.0 then "-" ^ result else result)
  | _ -> raise (CpsThrow ( "base got non-numbers"))

let char_at a b _ = match a, b with
  | String (_, _, s), Num (_, _, n) ->
    str (String.make 1 (String.get s (int_of_float n)))
  | _ -> raise (CpsThrow ( "char_at didn't get a string and a number"))

let locale_compare a b _ = match a, b with
  | String (_, _, r), String (_, _, s) ->
    num (float_of_int (String.compare r s))
  | _ -> raise (CpsThrow ( "locale_compare didn't get 2 strings"))

let pow a b _ = match a, b with
  | Num (_, _, base), Num (_, _, exp) -> num (base ** exp)
  | _ -> raise (CpsThrow ( "pow didn't get 2 numbers"))

let to_fixed a b _ = match a, b with
  | Num (_, _, x), Num (_, _, f) -> 
    let s = string_of_float x
    and fint = int_of_float f in
    if fint = 0 
      then str (string_of_int (int_of_float x)) 
      else let dot_index = String.index s '.'
      and len = String.length s in
      let prefix_chars = dot_index in
      let decimal_chars = len - (prefix_chars + 1) in
      if decimal_chars = fint then str s
      else if decimal_chars > fint
        then let fixed_s = String.sub s 0 (fint - prefix_chars) in
        str (fixed_s)
      else let suffix = String.make (fint - decimal_chars) '0' in
        str (s ^ suffix)
  | _ -> raise (CpsThrow ( "to-fixed didn't get 2 numbers"))

let rec is_accessor a b store = match a with
  | VarCell (_, _, a) -> (match (Store.find a store), b with
    | Object(_, _, {proto = p}, props), String (_, _, s) ->
      (try
         match List.assoc s props with
         | Data _ -> bool false
         | Accessor _ -> bool true
       with Not_found ->
         match p with None -> bool false | Some pr -> is_accessor pr b store)
    | Null _, String (_, _, s) -> raise (CpsThrow ( "isAccessor topped out"))
    | _ -> raise (CpsThrow ( "isAccessor")))
  | _ -> raise (CpsThrow ("[cps-interp] isAccessor didn't get a VarCell"))
      
let op2 op = match op with
  | "+" -> arith_sum
  | "-" -> arith_sub
  | "/" -> arith_div
  | "*" -> arith_mul
  | "%" -> arith_mod
  | "&" -> bitwise_and
  | "|" -> bitwise_or
  | "^" -> bitwise_xor
  | "<<" -> bitwise_shiftl
  | ">>" -> bitwise_shiftr
  | ">>>" -> bitwise_zfshiftr
  | "<" -> arith_lt
  | "<=" -> arith_le
  | ">" -> arith_gt
  | ">=" -> arith_ge
  | "stx=" -> stx_eq
  | "abs=" -> abs_eq
  | "hasProperty" -> has_property
  | "hasOwnProperty" -> has_own_property
  | "string+" -> string_plus
  | "string<" -> string_lessthan
  | "base" -> get_base
  | "char-at" -> char_at
  | "locale-compare" -> locale_compare
  | "pow" -> pow
  | "to-fixed" -> to_fixed
  | "isAccessor" -> is_accessor
  | _ -> failwith ("no implementation of binary operator: " ^ op)
