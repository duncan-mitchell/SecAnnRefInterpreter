open List
open Prelude
open Ljs
open Ljs_eval
(*open Ljs_cesk*)
open Ljs_syntax
open Ljs_annotation
(*open Ljs_pretty_html*)
open Reachability
(*open Ljs_fold_const
open Ljs_propagate_const
open Ljs_clean_deadcode
open Ljs_propagate_nonconst
open Ljs_inline_function
open Ljs_restore_id
open Ljs_clean_env
open Ljs_clean_assertion
open Ljs_convert_assignment
open Ljs_clean_assertion_harsh
open Ljs_restore_function
open Ljs_fix_arity
open Exp_util*)

type node =
  | Js of Js_syntax.program
  | Ejs of IdSet.t * Exprjs_syntax.expr
  | Ljs of Ljs_syntax.exp
(*| Cps of Ljs_cps.cps_exp *)
  | Env of (Ljs_syntax.exp -> Ljs_syntax.exp)
  | Answer of answer

type nodeType = JsT | EjsT | LjsT (*| CpsT *)| EnvT | AnswerT

let nodeType (node : node) : nodeType =
  match node with
  | Js _ -> JsT
  | Ejs _ -> EjsT
  | Ljs _ -> LjsT
(*| Cps _ -> CpsT *)
  | Env _ -> EnvT
  | Answer _ -> AnswerT


let showNodeType (nodeType : nodeType) : string =
  match nodeType with
  | JsT -> "JS"
  | EjsT -> "ExprJS"
  | LjsT -> "S5"
(*  | CpsT -> "S5-cps" *)
  | EnvT -> "S5-env"
  | AnswerT -> "Snapshot"


module S5 = struct

  open Format
  open Js_to_exprjs
  open Exprjs_to_ljs
  open Exprjs_syntax
  open Js_syntax
  open Ljs_desugar
  open Format
  open FormatExt
  open Ljs_gc

  module LocSet = Store.LocSet
  module LocMap = Store.LocMap


  (* Global Options *)

  let jsparser_path = ref "../scripts/jsparser.sh"
  let stack_trace = ref true

  let set_stack_trace (cmdName : string) (on : bool) : unit =
    stack_trace := on

  let set_json (cmdName : string) (path : string) : unit =
    jsparser_path := path


  (* Stack Operations *)

  let stack : node list ref = ref []

  module VarMap = Map.Make (String)
  let var_map = ref VarMap.empty

  let push (node : node) : unit =
    stack := node :: !stack

  let type_error (cmd : string) (expected : nodeType) (found : node) : 'a =
    failwith (cmd ^ ": expected " ^ showNodeType expected ^ ", but found " ^ showNodeType (nodeType found))

  let underflow_error (cmd : string) : 'a =
    failwith (cmd ^ ": stack underflow")

  let pop cmd : node =
    match !stack with
    | first :: rest ->
        stack := rest;
        first
    | _ -> underflow_error cmd

  let peek cmd : node =
    match !stack with
    | first :: rest -> first
    | _ -> underflow_error cmd

  let pop_js cmd : Js_syntax.program =
    match pop cmd with
    | Js src -> src
    | node -> type_error cmd JsT node

  let pop_ejs cmd : IdSet.t * Exprjs_syntax.expr =
    match pop cmd with
    | Ejs (used_ids, src) -> (used_ids, src)
    | node -> type_error cmd EjsT node

  let pop_ljs cmd : Ljs_syntax.exp =
    match pop cmd with
    | Ljs src -> src
    | node -> type_error cmd LjsT node

 (* let pop_cps cmd : Ljs_cps.cps_exp =
    match pop cmd with
    | Cps src -> src
    | node -> type_error cmd CpsT node *)

  let pop_env cmd : Ljs_syntax.exp -> Ljs_syntax.exp =
    match pop cmd with
    | Env src -> src
    | node -> type_error cmd EnvT node

  let pop_answer cmd : Ljs_eval.answer =
    match pop cmd with
    | Answer answer -> answer
    | node -> type_error cmd AnswerT node

  let push_js js = push (Js js)
  let push_ejs (used_ids, ejs) = push (Ejs (used_ids, ejs))
  let push_ljs ljs = push (Ljs ljs)
(*  let push_cps cps = push (Cps cps) *)
  let push_env env = push (Env env)
  let push_answer answer = push (Answer answer)

  let peek_answer cmd : Ljs_eval.answer =
    let ans = pop_answer cmd in
    push_answer ans;
    ans


  (* Pushing Commands *)

  let load_c3_js cmd path =
    push_js (C3.parse_c3 (open_in path) path)

  let load_desugared cmd path =
    let js_src = string_of_file path in
    try push_ljs (Ljs_desugar.parse_and_desugar !jsparser_path js_src)
    with Ljs_values.PrimErr (t, v) -> print_string
      ("Error while desugaring: " ^ Ljs_values.pretty_value v ^ "\n")

  let load_js cmd path =
    push_js (SpiderMonkey.parse_spidermonkey (open_in path) path)

  let load_ljs cmd path =
    push_ljs (Ljs.parse_es5 (open_in path) path)

  let load_env cmd path =
    push_env (Ljs.parse_es5_env (open_in path) path)

  (* FIXME(junsong): the internal_env is mainly for nested eval. It
   * interpolates es5.env IDs into environment, and has a huge negative
   * impact on constant folding. It should not be used with
   * optimization flags. I don't know what to do to integrate the two.*)
  let load_internal_env cmd name = match name with
    | "env-vars" ->
       push_env (Env_free_vars.vars_env)
    | _ -> failwith ("Unknown internal environment " ^ name)

  (* Conversion Commands *)

  let js_to_ejs cmd () =
    let convert js = js_to_exprjs Pos.dummy js (Exprjs_syntax.IdExpr (Pos.dummy, "global")) in
    push_ejs (convert (pop_js cmd))

  let ejs_to_ljs cmd () =
    let convert (used_ids, exprjsd) = exprjs_to_ljs Pos.dummy used_ids exprjsd in
    push_ljs (convert (pop_ejs cmd))

(*  let ljs_to_cps cmd () =
    let convert ljs = Ljs_cps.cps_tail ljs "%error" (Ljs_cps.RetId(Pos.dummy, Ljs_cps.Label.dummy, "%answer")) in
    push_cps (convert (pop_ljs cmd))

  let cps_to_ljs cmd () =
    push_ljs (Ljs_cps.de_cps (pop_cps cmd)) *)

  let ljs_to_env cmd () =
    let src1 = pop_ljs cmd in
    push_env (fun src2 -> Ljs_syntax.Seq (Pos.dummy, src1, src2))

  let collect_garbage cmd () =
    let answer = pop_answer cmd in
    match answer with
    | Ljs_eval.Answer (exps, v, envs, store) ->
        let root_set = LocSet.unions (map Ljs_gc.locs_of_env envs) in
        let store' = Ljs_gc.collect_garbage store root_set in
        push_answer (Ljs_eval.Answer (exps, v, envs, store'))


  (* Composition Commands *)

  let apply cmd () =
    let env = pop_env cmd in
    let ljs = pop_ljs cmd in
    push_ljs (env ljs)

  (* This should be defined such that E1 apply E2 apply = E1 E2 compose apply *)
  let compose cmd () =
    let env2 = pop_env cmd in
    let env1 = pop_env cmd in
    push_env (fun ljs -> env2 (env1 ljs))


  (* Printing Commands *)

  let print_literal cmd str =
    print_endline str

  let print_value cmd () =
    match peek_answer cmd with
    | Ljs_eval.Answer (_, value, _, _) ->
      print_endline (Ljs_values.pretty_value value)

  let print_env cmd () =
    match peek_answer cmd with
    | Ljs_eval.Answer (_, _, env, _) ->
      print_endline (Ljs_pretty_value.string_of_env (last env))

  let print_store cmd () =
    match peek_answer cmd with
    | Ljs_eval.Answer (_, _, _, store) ->
      Ljs_pretty_value.print_objects store;
      Ljs_pretty_value.print_values store
(*
  let print_store_as_html cmd () =
    let answer = peek_answer cmd in
    let title = "S5 Javascript Heap" in
    let stylefiles = ["style.css"] in
    let filter = {
      traverse_hidden_props = true;
      traverse_closures = true;
      primordials = LocSet.empty;
    } in
    let html = html_of_answer answer filter in
    let document = Html.document title stylefiles [] [html] in
    Html.print_document document

  let print_ses_store_as_html cmd opt =
    let ses_answer = pop_answer cmd in
    let init_answer = pop_answer cmd in
    push_answer init_answer;
    push_answer ses_answer;
    match init_answer with
    | Ljs_eval.Answer (_, _, _, (obj_store, var_store, ann_store)) ->
      let title = "SES Javascript Heap" in
      let stylefiles = ["style.css"] in
      let traverse_closures = match opt with
        | "closure" -> true
        | "noclosure" -> false
        | _ -> failwith ("S5: " ^ cmd ^ ": unrecognized print style option.") in
      let filter = {
        traverse_hidden_props = false;
        traverse_closures = traverse_closures;
        primordials = LocSet.from_list (Store.keys obj_store)
      } in
      let html = html_of_answer ses_answer filter in
      let document = Html.document title stylefiles [] [html] in
      Html.print_document document
*)
  let print_src cmd () =
    match peek cmd with
    | Ejs (used_ids, src) -> Exprjs_pretty.exp src std_formatter; print_newline ()
    | Ljs src -> Ljs_pretty.exp src std_formatter; print_newline ()
(*    | Cps src -> Ljs_cps_pretty.exp true src std_formatter; print_newline () *)
    | node -> failwith (cmd ^ ": print: unsupported source type" ^ showNodeType (nodeType node))

  let print_js_fvs cmd () =
    let js = pop_js cmd in
    let fvs = Js_syntax.used_vars_sel js in
    push_js js; (* put it back *)
    printf "%s\n" ((FormatExt.to_string (fun lst -> (vert (map text lst))))
                      (IdSet.elements fvs))

  (* Other Commands *)

(*  let alphatize cmd () =
    let alph cps = fst (Ljs_cps_util.alphatize true (cps, IdMap.add "%error" 0 (IdMap.add "%answer" 0 IdMap.empty))) in
    push_cps (alph (pop_cps cmd)) *)

  let save_answer cmd file_name =
    let ans = pop_answer cmd in
    Marshal.to_channel (open_out_bin file_name) ans []

  let load_answer cmd file_name =
    let ans = Marshal.from_channel (open_in_bin file_name) in
    push_answer ans

  let get_var cmd var =
    let x = VarMap.find var !var_map in
    push x

  let set_var cmd var =
    let x = pop cmd in
    var_map := VarMap.add var x !var_map

  let ses_check cmd () =
    let ses_ans = pop_answer cmd in
    let init_ans = pop_answer cmd in
    Heapwalk.ses_check init_ans ses_ans

  (* Evaluation Commands *)

(*  let cps_eval cmd () =
    let cps = pop_cps cmd in
    let v = Cfg.eval cps in
    (match v with
    | Cfg.Ans v -> printf "ANSWER %s" (Ljs_cps_values.pretty_bind v)
    | Cfg.Err v -> printf "ERROR %s" (Ljs_cps_values.pretty_bind v));
    print_newline ()

  let cps_eval_abs cmd () =
    let cps = pop_cps cmd in
    let module FX = FormatExt in
    let (finalEnv, finalStore, finalLab) = Cfg_abs.eval cps in
    printf "Finished evaling...finalLab is %s\n" (Ljs_cps.Label.toString finalLab);
    let ans = Cfg_abs.getBinding finalLab "%%ANSWER" finalEnv finalStore in
    let err = Cfg_abs.getBinding finalLab "%%ERROR" finalEnv finalStore in
    FX.vert [FX.horz [FX.text "ANSWER <="; Ljs_cps_absdelta.ValueLattice.pretty ans];
             FX.horz [FX.text "ERROR  <="; Ljs_cps_absdelta.ValueLattice.pretty err]] Format.str_formatter;
    printf "%s\n" (Format.flush_str_formatter ()) *)
(*
  let ljs_cesk cmd () =
    let ljs = pop_ljs cmd in
    let answer = Ljs_cesk.eval_expr ljs (parse_and_desugar !jsparser_path) !stack_trace in
    push_answer answer
  *)

  let ljs_eval cmd () =
    let ljs = pop_ljs cmd in
    let answer = Ljs_eval.eval_expr ljs (parse_and_desugar !jsparser_path) !stack_trace in
    push_answer answer
(*
  let continue_cesk_eval cmd () =
    let ljs = pop_ljs cmd in
    let Ljs_eval.Answer (_, _, envs, store) = pop_answer cmd in
    let answer = Ljs_cesk.continue_eval
      ljs (parse_and_desugar !jsparser_path) !stack_trace (last envs) store in
    push_answer answer
*)
  let continue_ljs_eval cmd () =
    let ljs = pop_ljs cmd in
    let Ljs_eval.Answer (_, _, envs, store) = pop_answer cmd in
    let answer = Ljs_eval.continue_eval
      ljs (parse_and_desugar !jsparser_path) !stack_trace (last envs) store in
    push_answer answer

(*  let do_sym_eval cmd =
    let ljs = pop_ljs cmd in
    let t1 = Sys.time() in
    let res = Ljs_sym_eval.eval_expr ljs !jsparser_path 50 Ljs_sym_values.mt_ctx in
    let t2 = Sys.time() in
    printf "Spent %f secs in sym eval\n" (t2 -. t1);
    res

  let ljs_sym_eval cmd () =
    (* let z3 = Unix.open_process "z3 -smt2 -in" in *)
    (* let (inch, outch) = z3 in begin *)
    let results = do_sym_eval cmd in
    Ljs_sym_z3.print_results results
  (* close_in inch; close_out outch *)

  let ljs_sym_eval_raw cmd () =
    let results = do_sym_eval cmd in
    print_string "RAW RESULTS"; print_newline();
    output_value stdout results;
    print_newline() *)

  (* optimization command *)
(*
  let opt_fold_const cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = fold_const ljs in
    push_ljs new_ljs
    (* print origin one for debug *)
    (*Ljs_pretty.exp ljs std_formatter;
    print_newline ()*)

  let opt_propagate_const cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = propagate_const ljs in
    push_ljs new_ljs

  let opt_clean_deadcode cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = clean_deadcode ljs in
    push_ljs new_ljs

  let opt_propagate_nonconst cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = propagate_nonconst ljs in
    push_ljs new_ljs

  let opt_inline_function cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = inline_function ljs in
    push_ljs new_ljs

  let opt_restore_id cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = restore_id ljs in
    push_ljs new_ljs

  let opt_clean_env cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = clean_env ljs in
    push_ljs new_ljs

  let opt_clean_assertion cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = clean_assertion ljs in
    push_ljs new_ljs

  let opt_convert_assignment cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = convert_assignment ljs in
    push_ljs new_ljs

  let opt_clean_assertion_harsh cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = clean_assertion_harsh ljs in
    push_ljs new_ljs

  let opt_restore_function cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = restore_function ljs in
    push_ljs new_ljs

  let opt_fix_arity cmd () =
    let ljs = pop_ljs cmd in
    let new_ljs = fix_arity ljs in
    push_ljs new_ljs

  (* measure function accepts a function that does actual counting (measuring),
     and output the environment and user measurement, separately.
  *)
  let measure cmd (str : string) (count_function : exp -> int) =
    let ljs = pop_ljs cmd in
    let total = count_function ljs in
    let usercode_n = apply_to_user_code ljs count_function in
    let envn, usern = 
      if usercode_n = 0 then (* no env delimitor *)
        0, total - usercode_n
      else
        total - usercode_n, usercode_n in
    begin
      print_string str; printf ": env(%d);usr(%d)\n" envn usern;
      push_ljs ljs;
    end

  let count_nodes cmd (str : string) =
    let rec count (e : exp) : int =
      (* shrinkage change the object model, function signature, etc.
         Counting must reflect these node *)
      let data_count {value=x; writable=_} = 1 + (count x) in
      let acc_count {getter=x; setter=y} = (count x) + (count y) in
      let prop_count (prop : Ljs_syntax.prop) = match prop with
        | Ljs_syntax.Data (data, _, _) -> 2 + data_count data
        | Ljs_syntax.Accessor (acc, _, _) -> 2 + acc_count acc
      in
      let prop_list_count props = 
        List.fold_left (+) 0 (List.map (fun (_, prop) -> 
                                          1 + (prop_count prop)) props)
      in
      let option_count option = match option with
        | None -> 0
        | Some x -> count x
      in
      let attrs_count attrs = match attrs with
        | {primval=e1; code=e2; proto=e3;klass=_;extensible=_} ->
         2 + (option_count e1) + (option_count e2) + (option_count e3)
      in
      match e with
      | Object (_, attrs, props) ->
        (attrs_count attrs) + (prop_list_count props)
      | Let (_, _, xv, body) -> 1 + (count xv) + (count body)
      | Rec (_, _, xv, body) -> 1 + (count xv) + (count body)
      | Lambda (_, xs, body) -> (List.length xs) + (count body)
      | SetBang (_, _, v) -> 1 + (count v)
      | _ -> 1 + (List.fold_left (+) 0 (List.map count (child_exps e))) in
    measure cmd str count

  let count_max_depth cmd (str : string) =
    let max_in_list (lst : 'a list) =
      let rec max_iter m l = match l with
        | [] -> m
        | a :: tail ->
          if a > m then
            max_iter a tail
          else
            max_iter m tail
      in
      max_iter 0 lst
    in
    let rec count (e : exp) : int =
      1 + (max_in_list (List.map count (child_exps e)))
    in
    measure cmd str count
    
  let print_user_s5 cmd () =
    match peek cmd with
    | Ljs src ->
      apply_to_user_code src
        (fun e ->
           Ljs_pretty.exp e std_formatter;
           print_newline ();)
    | _ -> failwith "print-user-s5 only supports printing s5 code"

  let save_s5 cmd (filename : string) =
    let ljs = pop_ljs cmd in
    push_ljs ljs;
    Marshal.to_channel (open_out_bin filename) ljs []

  let load_s5 cmd (filename : string) =
    let ljs = Marshal.from_channel (open_in_bin filename) in
    push_ljs ljs
*)
  (* Main *)

  let command spec optName func desc = (optName, spec (func optName), desc)
  let strCmd = command (fun cmd -> Arg.String cmd)
  let boolCmd = command (fun cmd -> Arg.Bool cmd)
  let unitCmd = command (fun cmd -> Arg.Unit cmd)
  let showType fromTypes toTypes =
    let showTypeList types = String.concat " " (List.map showNodeType types) in
    "(" ^ showTypeList fromTypes ^ " -> " ^ showTypeList toTypes ^ ")"
  let main () : unit =
    Arg.parse
      [
        (* Global Options *)
        strCmd "-set-json" set_json
          "<file> set the path to a script that converts js to json";
        boolCmd "-set-stack-trace" set_stack_trace
          "<bool> enable/disable stack traces from the interpreter";
        (* Loading *)
        strCmd "-desugar" load_desugared "<file> load Javascript as S5";
        strCmd "-js" load_js "<file> load file as JavaScript AST";
        strCmd "-c3-js" load_c3_js "<file> load javascript using C3";
        strCmd "-s5" load_ljs "<file> load file as S5";
        strCmd "-env" load_env "<file> load file as S5-env";
        strCmd "-internal-env" load_internal_env
          ("[env-vars] load an internal environment as S5-env.  " ^
          "Options are currently only env-vars, which sets up the " ^
            "global environment for nested evals (this flag shoud " ^
              "not be used with optimization flags)");
        (* Conversion *)
        unitCmd "-js-to-ejs" js_to_ejs (showType [JsT] [EjsT]);
        unitCmd "-ejs-to-s5" ejs_to_ljs (showType [EjsT] [LjsT]);
        unitCmd "-js-to-s5"
          (fun cmd () -> js_to_ejs cmd (); ejs_to_ljs cmd ())
          (showType [JsT] [LjsT]);
        (*unitCmd "-cps" ljs_to_cps (showType [LjsT] [CpsT]);
        unitCmd "-un-cps" cps_to_ljs (showType [CpsT] [LjsT]);*)
        unitCmd "-to-env" ljs_to_env (showType [LjsT] [EnvT]);
        unitCmd "-gc" collect_garbage (showType [AnswerT] [AnswerT]);
        (* Composition Operations *)
        unitCmd "-apply" apply (showType [LjsT; EnvT] [LjsT]);
        unitCmd "-compose" compose (showType [EnvT; EnvT] [EnvT]);
(*        unitCmd "-diff" diff (showType [AnswerT; AnswerT] [AnswerT]); *)
        (* Printing *)
        strCmd "-print-string" print_literal
          "Output a literal string (useful for delimiting other print statements)";
        unitCmd "-print-val" print_value
          "print the value resulting from evaluation";
        unitCmd "-print-env" print_env
          "print the environment (id to store location mapping)";
        unitCmd "-print-src" print_src
          "pretty-print s5 or exprjs code";
(*        unitCmd "-print-user-s5" print_user_s5 
          "pretty-user user code of the s5 code"; *)
        unitCmd "-print-fvs" print_js_fvs
          "print JavaScript free variables";
        unitCmd "-print-heap" print_store
          "print objects and values in the heap after evaluation";
(*        unitCmd "-print-html" print_store_as_html
          "print objects and values in the heap as html";
        strCmd "-print-ses-html" print_ses_store_as_html
          "<string> print ses heap as html. init_heap ses_heap -> init_heap ses_heap";*)
        (* Other *)
        unitCmd "-ses-check" ses_check
          "Check if ses ran properly. ANS(init) ANS(ses) -> ";
 (*       unitCmd "-alph" alphatize
          "Alpha-convert the CPS representation"; *)
        strCmd "-save" save_answer
          "<file> save the heap and environment in the specified file";
        strCmd "-load" load_answer
          "<file> load the heap and environment from a file";
        strCmd "-set" set_var
          "<VAR> Pop something off the stack and save it under the name VAR";
        strCmd "-get" get_var
          "<VAR> Push the thing saved as VAR onto the stack (use this after -set)";
        (* Evaluation *)
        unitCmd "-eval" (fun cmd () -> ljs_eval cmd (); print_value cmd ())
          "evaluate S5 code and print the result";
        unitCmd "-continue-s5-eval"
          (fun cmd () -> continue_ljs_eval cmd (); print_value cmd ())
          (showType [AnswerT; LjsT] [AnswerT]);
        unitCmd "-eval-s5" ljs_eval
          "evaluate S5 code";
(*        unitCmd "-continue-cesk-eval"
          (fun cmd () -> continue_cesk_eval cmd (); print_value cmd ())
          (showType [AnswerT; LjsT] [AnswerT]);
        unitCmd "-eval-cesk" ljs_cesk
          "evaluate S5 code using a CESK";*)
(*        unitCmd "-eval-cps" cps_eval
          "evaluate code in CPS form"; 
        unitCmd "-eval-cps-abs" cps_eval_abs
          "abstractly evaluate code in CPS form"; *)
(*        unitCmd "-sym-eval" ljs_sym_eval
          "evaluate code symbolically";
        unitCmd "-sym-eval-raw" ljs_sym_eval_raw
          "evaluate code symbolically and print raw OCaml results"; *)
        (* optimization *)
(*        strCmd "-save-s5" save_s5
          "marshal s5 code to file as sequence of bytes";
        strCmd "-load-s5" load_s5
          "load s5 from marshalled file that created by -save-s5(use -s5 to load text form of s5 code)";
        unitCmd "-opt-fold-const" opt_fold_const
          "perform constant folding on s5";
        unitCmd "-opt-propagate-const" opt_propagate_const
          "perform constant propagation on s5";
        unitCmd "-opt-clean-deadcode" opt_clean_deadcode
          "clean unused bindings in s5";
        unitCmd "-opt-propagate-nonconst" opt_propagate_nonconst
          "propagate single-use functions, objects, and let bindings in s5";
        unitCmd "-opt-inline-function" opt_inline_function
          "perform function inlining on s5";
        unitCmd "-opt-clean-assertion" opt_clean_assertion
          "clean static checks as much as possible";
        unitCmd "-opt-convert-assignment" opt_convert_assignment
          "convert assignment to let bindings when possible";
        unitCmd "-opt-restore-id" opt_restore_id
          "[semantics altering] restore JavaScript id in desugared S5";
        unitCmd "-opt-clean-assertion-harsh" opt_clean_assertion_harsh
          "[semantics altering] clean all static checks";
        unitCmd "-opt-restore-function" opt_restore_function
          "[semantics altering] restore function objects to procedures";
        unitCmd "-opt-clean-env" opt_clean_env
          "[semantics altering] clean unused env expression";
        unitCmd "-opt-fix-arity" opt_fix_arity
          "[semantics altering] disable variable function arity";
        strCmd "-count-nodes" count_nodes
          "count the nodes of S5";
        strCmd "-count-max-depth" count_max_depth
          "count the max depth of the S5 syntax tree"
*)          
      ]
      (load_ljs "-s5")
      ("Usage: s5 <action> <path> ...\n"
       ^ "  After loading, sources are held on a stack.\n"
       ^ "  There are five types: "
       ^ String.concat ", " (List.map showNodeType [JsT; EjsT; LjsT; (*CpsT;*) EnvT]) ^ "\n");;

end;;
S5.main ()
