(**********************************************************************)
(*                                                                    *)
(*              This file is part of the HOCL package                 *)
(*                                                                    *)
(*  Copyright (c) 2019-present, Jocelyn SEROT (jocelyn.serot@uca.fr). *)
(*                     All rights reserved.                           *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(* The lexer definition *)

{
open Parser

type lexical_error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

exception Lexical_error of lexical_error * int * int

(* The table of keywords *)

let keyword_table = [
  "type", TYPE;
  "nat", TY_NAT;
  "bool", TY_BOOL;
  "unit", TY_UNIT;
  "bcast", BCAST;
  "delay", DELAY;
  "actor", ACTOR;
  "parameter", PARAMETER;
  "param", PARAM;
  "source", SOURCE;
  "sink", SINK;
  "graph", GRAPH;
  "fun", FUN;
  "let", LET;
  "and", AND;
  "in", IN;
  "out", OUT;
  "rec", REC;
  (* "list", LIST; *)
  "match", MATCH;
  "with", WITH;
  "true", TRUE;
  "false", FALSE;
  "if", IF;
  "then", THEN;
  "else", ELSE;
  (* "initially", INITIALLY; *)
]

(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0;
  ()

let store_string_char c =
  if !string_index >= Bytes.length (!string_buff) then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
      Bytes.blit (!string_buff) 0 new_buff 0 (Bytes.length (!string_buff));
      string_buff := new_buff
  end;
  Bytes.set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = Bytes.sub (!string_buff) 0 (!string_index) in
    string_buff := initial_string_buffer;
    s

(* To translate escape sequences *)

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c
;;

let char_for_decimal_code lexbuf i =
  let c = 
    100 * (int_of_char(Lexing.lexeme_char lexbuf i) - 48) +
     10 * (int_of_char(Lexing.lexeme_char lexbuf (i+1)) - 48) +
          (int_of_char(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  char_of_int(c land 0xFF)
;;
}

rule main = parse
    [' ' '\010' '\013' ] +
      { main lexbuf }
  | ['A'-'Z' 'a'-'z' ]
    ( ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_' ] ) *
      { let s = Lexing.lexeme lexbuf in
          try
            List.assoc s keyword_table
          with Not_found ->
            IDENT s }
  | ['0'-'9']+
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "_" { UNDERSCORE }
  | "|" { BAR }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "*" { STAR }
  | "," { COMMA }
  | "->" { ARROW }
  | ";" { SEMI }
  | ":" { COLON }
  | "=" { EQUAL }
  | "::" { COLONCOLON }
  | "|>"    { INFIX0 "|>" }
  | ">>"    { INFIX0 ">>" }
  | "!="    { INFIX1 "!=" }
  | [ '=' '<' '>' ]
            { INFIX1(Lexing.lexeme lexbuf) }
  | [ '+' '-' ] 
            { INFIX2(Lexing.lexeme lexbuf) }
  | [ '*' '/' '%' ]
            { INFIX3(Lexing.lexeme lexbuf) }
  | "\""
      { reset_string_buffer();
        let string_start = lexbuf.Lexing.lex_start_pos + lexbuf.Lexing.lex_abs_pos in
        begin try
          string lexbuf
        with Lexical_error(Unterminated_string, _, string_end) ->
          raise(Lexical_error(Unterminated_string, string_start, string_end))
        end;
        lexbuf.Lexing.lex_start_pos <- string_start - lexbuf.Lexing.lex_abs_pos;
        STRING (Bytes.to_string (get_stored_string())) }
  | "--"
      { comment !Location.input_lexbuf; main !Location.input_lexbuf }
  | "#pragma" { PRAGMA }
  | eof { EOF }
  | _
      { raise (Lexical_error(Illegal_character,
                            Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and string = parse
    '"' (*"'"'*)
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' (*"'"'*) 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Lexical_error
                (Unterminated_string, 0, Lexing.lexeme_start lexbuf)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and comment = parse
  | "\n"
      { () }
  | eof
      { () }
  | _
      { comment !Location.input_lexbuf }
