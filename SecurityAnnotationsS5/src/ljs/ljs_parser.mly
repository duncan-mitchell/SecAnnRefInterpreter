%{
open Prelude
open Ljs_syntax
open Ljs_annotation

let true_c (p, annotation) = True (p, annotation)
let false_c (p, annotation) = False (p, annotation)

let undef_c p = Undefined p

let str (p, s, annotation) = String (p, s, annotation)

let num_c (p, d, annotation) = Num (p, d, annotation)
let int_c (p, d, annotation) = Num (p, float_of_int d, annotation)

(* Macros for expanding arguments objects and function objects (a
little bit of desugaring)*)

let rec mk_val p v =
  Data ({ value = v; writable = true }, true, true)

let rec mk_field (p, s, e) =
  (p, s, mk_val p e)

let with_pos exp pos = match exp with
  | SecAnn (_, annotation) -> SecAnn (pos, annotation)
  | Extends (_, a1, a2) -> Extends (pos, a1, a2)
  | Null _ -> Null pos
  | Undefined _ -> Undefined pos
  | String (_, str, annotation) -> String (pos, str, annotation)
  | Num (_, num, annotation) -> Num (pos, num, annotation)
  | True (_, annotation) -> True (pos, annotation)
  | False (_, annotation) -> False (pos, annotation)
  | Id (_, id) -> Id (pos, id)
  | Object (_, attrs, props, ann) -> Object (pos, attrs, props, ann)
  | GetAttr (_, pattr, obj, field) -> GetAttr (pos, pattr, obj, field)
  | SetAttr (_, prop, obj, field, value) -> SetAttr (pos, prop, obj, field, value)
  | GetObjAttr (_, oattr, obj) -> GetObjAttr (pos, oattr, obj)
  | SetObjAttr (_, oattr, obj, v) -> SetObjAttr (pos, oattr, obj, v)
  | GetField (_, left, right, args) -> GetField (pos, left, right, args)
  | SetField (_, obj, field, value, args) -> SetField (pos, obj, field, value, args)
  | DeleteField (_, obj, field) -> DeleteField (pos, obj, field)
  | OwnFieldNames (_, obj) -> OwnFieldNames(pos, obj)
  | SetBang (_, id, exp) -> SetBang (pos, id, exp)
  | Op1 (_, op, exp) -> Op1 (pos, op, exp)
  | Op2 (_, op, left, right) -> Op2 (pos, op, left, right)
  | If (_, test, trueBlock, falseBlock) -> If (pos, test, trueBlock, falseBlock)
  | App (_, func, args) -> App (pos, func, args)
  | Seq (_, left, right) -> Seq (pos, left, right)
  | Let (_, id, value, body) -> Let (pos, id, value, body)
  | Rec (_, id, value, body) -> Rec (pos, id, value, body)
  | Label (_, id, exp) -> Label (pos, id, exp)
  | Break (_, id, exp) -> Break (pos, id, exp)
  | TryCatch (_, tryBlock, catchBlock) -> TryCatch (pos, tryBlock, catchBlock)
  | TryFinally (_, tryBlock, finallyBlock) -> TryFinally (pos, tryBlock, finallyBlock)
  | Throw (_, value) -> Throw (pos, value)
  | Lambda (_, ids, body) -> Lambda (pos, ids, body)
  | LambdaEnf (_, ids_annotated, body) -> LambdaEnf (pos, ids_annotated, body)
  | As (_, e, ann) -> As (pos, e, ann)
  | Drop (_, e, ann) -> Drop (pos, e, ann)
  | CpAnn (_, e1, e2) -> CpAnn (pos, e1, e2)
  | Eval (_, exp, obj) -> Eval (pos, exp, obj)
  | Hint (_, label, exp) -> Hint (pos, label, exp)

%}

%token <int> INT
%token <float> NUM
%token <string> STRING
%token <string> HINT
%token <bool> BOOL
%token <string> ANNOTATION
%token <Prelude.id> ID
%token UNDEFINED NULL FUNC LET DELETE LBRACE RBRACE LPAREN RPAREN LBRACK
  RBRACK EQUALS COMMA DEREF REF COLON COLONEQ PRIM IF ELSE SEMI
  LABEL BREAK TRY CATCH FINALLY THROW EQEQEQUALS TYPEOF
  AMPAMP PIPEPIPE RETURN BANGEQEQUALS FUNCTION REC WRITABLE GETTER SETTER
  CONFIG VALUE ENUM LT GT PROTO CODE EXTENSIBLE CLASS EVAL GETFIELDS PRIMVAL
  AS DROP CPANN SECANN EXTENDS


%token EOF
%left COLONEQ
%left PIPEPIPE
%left AMPAMP
%left EQEQEQUALS BANGEQEQUALS
%left LBRACK
%left ELSE
%left LPAREN

%type <Ljs_syntax.exp> prog
%type <Ljs_syntax.exp -> Ljs_syntax.exp> env
%type <Ljs_syntax.exp> decl

%start decl
%start prog
%start env


%%

decl :
 | SECANN ANNOTATION EXTENDS ANNOTATION { Extends (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), parseAnn $2, parseAnn $4) }
 | SECANN ANNOTATION { SecAnn (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), parseAnn $2) }

const :
 | NUM ANNOTATION { Num (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1), $1, parseAnn $2) }
 | INT ANNOTATION {  Num (Pos.real (Parsing.rhs_start_pos 1, Parsing. rhs_end_pos 1), (float_of_int $1), parseAnn $2) }
 | NUM { Num (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1), $1, [Top]) }
 | INT {  Num (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1), (float_of_int $1), [Top]) }
 | STRING ANNOTATION { String (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1), $1, parseAnn $2) }
 | STRING {  String (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1), $1, [Top]) }
 | UNDEFINED { Undefined (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1)) }
 | NULL { Null (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1)) }
 | BOOL ANNOTATION
    { if $1 
          then True ((Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1)), parseAnn $2) 
          else False ((Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1)), parseAnn $2) }
 | BOOL
    { if $1 
          then True ((Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1)), [Top]) 
          else False ((Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1)), [Top]) }

more_oattrs :
 | COMMA oattrsv { $2 }
 | COMMA { d_attrs }
 | { d_attrs }

oattrsv :
 | PRIMVAL COLON exp more_oattrs { { $4 with primval = Some $3 } }
 | EXTENSIBLE COLON BOOL more_oattrs { { $4 with extensible = $3 } }
 | PROTO COLON exp more_oattrs { { $4 with proto = Some $3 } }
 | CODE COLON exp more_oattrs { {$4 with code = Some $3 } }
 | CLASS COLON STRING more_oattrs { { $4 with klass = $3 } }

oattr_name :
 | PRIMVAL { Primval }
 | PROTO { Proto }
 | CODE { Code }
 | CLASS { Klass }
 | EXTENSIBLE { Extensible }

attr_name :
 | WRITABLE { Writable }
 | CONFIG { Config }
 | VALUE { Value }
 | SETTER { Setter }
 | GETTER { Getter }
 | ENUM { Enum }

prop_attrs :
 | VALUE exp COMMA WRITABLE BOOL
     { Data ({ value = $2; writable = $5 }, false, false) }
 | VALUE exp COMMA WRITABLE BOOL COMMA CONFIG BOOL
     { Data ({ value = $2; writable = $5 }, false, $8) }
 | VALUE exp COMMA WRITABLE BOOL COMMA CONFIG BOOL COMMA ENUM BOOL
     { Data ({ value = $2; writable = $5 }, $11, $8) }
 | VALUE exp COMMA WRITABLE BOOL COMMA ENUM BOOL COMMA CONFIG BOOL
     { Data ({ value = $2; writable = $5 }, $8, $11) }
 | GETTER exp COMMA SETTER exp
     { Accessor ({ getter = $2; setter = $5 }, false, true) }

prop :
 | STRING COLON LBRACE prop_attrs RBRACE { ($1, $4) }
 | ID COLON LBRACE prop_attrs RBRACE { ($1, $4) }

props :
 | { [] }
 | prop { [$1] }
 | prop COMMA props { $1 :: $3 }

exps :
 | { [] }
 | unbraced_seq_exp { [$1] }
 | unbraced_seq_exp COMMA exps { $1 :: $3 }

ids :
 | { [] }
 | ID { [$1] }
 | ID COMMA ids { $1 :: $3 }

 ids_annotated :
 | { [] }
 | ID COLON ANNOTATION { [($1, parseAnn $3)] }
 | ID COLON ANNOTATION COMMA ids_annotated { ($1, parseAnn $3) :: $5}
 /*Legacy Formatting from APLAS Submission - no COLON*/
 | ID ANNOTATION { [($1, parseAnn $2)] }
 | ID ANNOTATION COMMA ids_annotated { ($1, parseAnn $2) :: $4}


func :
 | FUNC LPAREN ids RPAREN braced_seq_exp
   { Lambda (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 5), $3, $5) }
 | FUNC LPAREN ids_annotated RPAREN braced_seq_exp
   { LambdaEnf (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 5), $3, $5) }

atom :
 | const { $1 }
 | ID { Id (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 1), $1) }
 | LBRACE LBRACK oattrsv RBRACK props RBRACE ANNOTATION
   { Object (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6), $3, $5, parseAnn $7 )}
 | LBRACE LBRACK RBRACK props RBRACE ANNOTATION
   { Object (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6), d_attrs, $4, parseAnn $6 )}
 | LBRACE LBRACK oattrsv RBRACK props RBRACE 
   { Object (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6), $3, $5, [Top])}
 | LBRACE LBRACK RBRACK props RBRACE 
   { Object (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6), d_attrs, $4, [Top] )}
 | braced_seq_exp
   { $1 }
 | LPAREN unbraced_seq_exp RPAREN { $2 }
 | func { $1 }
 | TYPEOF atom
     { Op1 (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 2), "typeof", $2) }
     
exp :
 | decl { $1 }
 | atom { $1 }
 | exp LPAREN exps RPAREN 
   { App (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4), $1, $3) }
 | GETFIELDS LPAREN exp RPAREN
   { OwnFieldNames (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4), $3) }
 | EVAL LPAREN exp COMMA exp RPAREN
     { Eval (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4), $3, $5) }
 | PRIM LPAREN STRING COMMA unbraced_seq_exp COMMA unbraced_seq_exp RPAREN
   { Op2 (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 8), $3, $5, $7) }
 | PRIM LPAREN STRING COMMA unbraced_seq_exp RPAREN
   { Op1 (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6), $3, $5) }
 | ID COLONEQ exp
   { SetBang (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $1, $3) }
 | exp EQEQEQUALS exp
     { Op2 (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), "stx=", $1, $3) }
 | exp BANGEQEQUALS exp
     { let p = Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3) in
         If (p, Op2 (p, "stx=", $1, $3),
             False (p, [Top]), True (p, [Top])) }
 | exp LBRACK unbraced_seq_exp EQUALS unbraced_seq_exp RBRACK
   { let p = Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6) in
       Let (p, "$newVal", $5,
	     SetField (p, $1, $3, 
		       Id (p, "$newVal"), 
		       Object (p, d_attrs,
            [("0", Data ({ value = Id (p, "$newVal");
                          writable = true },
              true, true))], [Top])))
    }
 | exp LBRACK unbraced_seq_exp EQUALS unbraced_seq_exp COMMA unbraced_seq_exp RBRACK
   { SetField (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 8), $1, $3, $5, $7) }
 | exp LBRACK unbraced_seq_exp RBRACK
   { let p = Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4) in
     GetField (p, $1,  $3,
		       Object (p, d_attrs,
            [], [Top])) }
 | exp LBRACK unbraced_seq_exp COMMA unbraced_seq_exp RBRACK
   { GetField (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6), $1, $3, $5) }
 | exp LBRACK DELETE unbraced_seq_exp RBRACK
     { DeleteField (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 5), $1, $4) }
 | exp LBRACK unbraced_seq_exp LT attr_name GT RBRACK
     { GetAttr (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 7), $5, $1, $3) }
 | exp LBRACK unbraced_seq_exp LT attr_name GT EQUALS unbraced_seq_exp RBRACK
     { SetAttr (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 9), $5, $1, $3, $8) }
 | exp LBRACK LT oattr_name GT RBRACK
     { GetObjAttr(Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 6),
                  $4, $1) }
 | exp LBRACK LT oattr_name GT EQUALS unbraced_seq_exp RBRACK
     { SetObjAttr(Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 8),
                  $4, $1, $7) }
 | exp AMPAMP exp
     { If (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $1, 
            $3, False ((Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3)), [Top])) }
 | exp PIPEPIPE exp
     { let p = Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3) in
         Let (p, "%or", $1,
               If (p, Id (p, "%or"), Id (p, "%or"), $3)) }
 | exp AS ANNOTATION
     { As (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $1, parseAnn $3) }
 | exp DROP ANNOTATION
      { Drop (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $1, parseAnn $3) }
 | CPANN LPAREN exp COMMA exp RPAREN
      { CpAnn (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $3, $5) }
 /*Legacy Formatting from APLAS Submission - purely functional style*/
 | CPANN exp exp
      { CpAnn (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $2, $3) }

cexp :
 | exp { $1 }
 | ifexp { $1 }
 | LPAREN HINT cexp RPAREN
     { Hint (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4), $2, $3) }
 | LABEL ID COLON braced_seq_exp
     { Label (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4), $2, $4) } 
 | BREAK ID cexp
   { Break (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $2, $3) }
 | THROW cexp
   { Throw (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 2), $2) }
 | TRY braced_seq_exp CATCH braced_seq_exp
   { TryCatch (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4), $2, $4) }
 | TRY braced_seq_exp FINALLY braced_seq_exp
   { TryFinally (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 4), $2, $4) }

ifexp :
 | IF LPAREN unbraced_seq_exp RPAREN braced_seq_exp ELSE ifexp
     { If (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 7), $3, $5, $7) }
 | IF LPAREN unbraced_seq_exp RPAREN braced_seq_exp ELSE braced_seq_exp
     { If (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 7), $3, $5, $7) }
 | IF LPAREN unbraced_seq_exp RPAREN braced_seq_exp
     { If (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 5), $3, $5, 
	    Undefined (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 5))) }

braced_seq_exp :
 | LBRACE unbraced_seq_exp RBRACE { with_pos $2 (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3)) }

unbraced_seq_exp :
 | unbraced_seq_item SEMI unbraced_seq_exp { Seq (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3), $1, $3) }
 | unbraced_seq_item { $1 }

unbraced_seq_item :
 | cexp { $1 }
 | LET LPAREN ID EQUALS unbraced_seq_exp RPAREN unbraced_seq_item
   { Let (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 7), $3, $5, $7) }
 | REC LPAREN ID EQUALS func RPAREN unbraced_seq_item
   { Rec (Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 7), $3, $5, $7) }

env :
 | EOF
     { fun x -> x }
 | LET LBRACK ID RBRACK EQUALS unbraced_seq_exp env
     { let p = Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 7) in
         fun x -> 
           Let (p, $3, $6, $7 x) }
 | REC LBRACK ID RBRACK EQUALS unbraced_seq_exp env
     { let p = Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 7) in
         fun x -> 
           Rec (p, $3, $6, $7 x) }
 | braced_seq_exp env
     { let p = Pos.real (Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 2) in
         fun x -> Seq (p, $1, $2 x) }


prog :
 | unbraced_seq_exp EOF { $1 }
%%
