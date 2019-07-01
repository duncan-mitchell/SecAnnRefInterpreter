{
open Prelude
open Lexing
open Ljs_syntax
open Ljs_parser

module S = String

let comment_start_p = ref dummy_pos

let block_comment_buf = Buffer.create 120

(* TODO: if integer conversions overflow, treat as a float *)
let parse_num_lit (s : string) : token =
  if S.contains s 'x' || S.contains s 'X'
    then INT (int_of_string s)
    else if S.contains s '.'
           then NUM (float_of_string s)
           else if S.contains s 'e' || S.contains s 'E'
                  then NUM (float_of_string s)
                  else INT (int_of_string s)
}

(* dec_digit+ corresponds to DecimalDigits in the spec. *)
let dec_digit = ['0'-'9']

let signed_int = dec_digit+ | ('+' dec_digit+) | ('-' dec_digit+)

let expt_part = ['e' 'E'] signed_int

let dec_int_lit = '0' | (['1'-'9'] dec_digit*)

let hex = ['0'-'9' 'A'-'f' 'a'-'f']

let hex_lit = ("0x" | "0X") hex+

let dec_lit = 
  (signed_int '.' dec_digit* expt_part?) | 
  ('.' dec_digit+ expt_part?) |
  (dec_int_lit expt_part?)

let num_lit = dec_lit | hex_lit

let ident = ['a'-'z' 'A'-'Z' '$' '_' '%' '#']['%']?['a'-'z' 'A'-'Z' '0'-'9' '$' '_' '-']*

let digit = ['0'-'9']

let char = [^ '"' '\\']

let blank = [ ' ' '\t' ]

let escape_sequence
  = [^ '\r' '\n'] | ('x' hex hex) | ('u' hex hex hex hex)

let double_quoted_string_char = 
  [^ '\r' '\n' '"' '\\'] | ('\\' escape_sequence)

let single_quoted_string_char =
  [^ '\r' '\n' '\'' '\\'] | ('\\' escape_sequence)

let angled_annotation_char = 
  [^ '\r' '\n' '<' '>' '\\'] | ('\\' escape_sequence)


rule token = parse
   | blank + { token lexbuf }
   | '\n' { new_line lexbuf; token lexbuf }
   | "/*" {  block_comment lexbuf }
   | "//"[^ '\r' '\n']* [ '\r' '\n' ] { new_line lexbuf; token lexbuf }

   | "/*:" { comment_start_p := lexeme_start_p lexbuf;
             hint lexbuf }
   | '"' (double_quoted_string_char* as x) '"' { STRING x }
   | ''' (single_quoted_string_char* as x) ''' { STRING x }
  
   | num_lit as x { parse_num_lit x }
   | "NaN" { NUM nan }
   | "+inf" { NUM infinity }
   | "-inf" { NUM neg_infinity }
   | "{" { LBRACE }
   | "}" { RBRACE }
   | '(' { LPAREN }
   | ')' { RPAREN }
   | "undefined" { UNDEFINED }
   | "null" { NULL }
   | "func" { FUNC }
   | "let" { LET }
   | "rec" { REC }
   | "delete" { DELETE }
   | "[" { LBRACK }
   | "]" { RBRACK }
   | "<" { LT }
   | ">" { GT }
   | "=" { EQUALS }
   | "," { COMMA }
   | "!" { DEREF }
   | "ref" { REF }
   | ":" { COLON }
   | ":=" { COLONEQ }
   | "prim" { PRIM }
   | "if" { IF }
   | "else" { ELSE }
   | ";" { SEMI }
   | "label" { LABEL }
   | "break" { BREAK }
   | "try" { TRY }
   | "catch" { CATCH }
   | "finally" { FINALLY }
   | "throw" { THROW }
   | "@eval" { EVAL }
   | "===" { EQEQEQUALS }
   | "!==" { BANGEQEQUALS }
   | "typeof" { TYPEOF }
   | "true" { BOOL true }
   | "false" { BOOL false}
   | "&&" { AMPAMP }
   | "||" { PIPEPIPE }
   | "return" { RETURN }
   | "function" { FUNCTION }
   | "#configurable" { CONFIG }
   | "#setter" { SETTER }
   | "#getter" { GETTER }
   | "#writable" { WRITABLE }
   | "#value" { VALUE }
   | "#enumerable" { ENUM }
   | "#proto" { PROTO }
   | "#code" { CODE }
   | "#extensible" { EXTENSIBLE }
   | "#primval" { PRIMVAL }
   | "#class" { CLASS }
   | "get-own-field-names" { GETFIELDS }
   | "<!" (angled_annotation_char* as x) "!>" { ANNOTATION x }  
   | "as" { AS }  
   | "drop" { DROP }
   | "cpAnn" { CPANN }
   | "SecAnn" { SECANN }
   | "Extends" { EXTENDS }

   | ident as x { ID x }
 
   | eof { EOF }

and block_comment = parse
    "*/" { token lexbuf }
  | '*' { block_comment lexbuf }
  | [ '\n' '\r' ]  {  block_comment lexbuf }
  | [^ '\n' '\r' '*']+ { block_comment lexbuf }

and hint = parse
  | "*/" { let str = Buffer.contents block_comment_buf in
             Buffer.clear block_comment_buf; HINT str }
  | '*' { Buffer.add_char block_comment_buf '*'; hint lexbuf }
  | "\r\n" { new_line lexbuf; Buffer.add_char block_comment_buf '\n'; 
             hint lexbuf }
  | [ '\n' '\r' ] { new_line lexbuf; Buffer.add_char block_comment_buf '\n';
                    hint lexbuf }
  | ([^ '\n' '\r' '*'])+ as txt { Buffer.add_string block_comment_buf txt;
                                  hint lexbuf }
