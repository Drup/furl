open Rresult

exception Not_implemented

(** Types *)

type operator =
  | No
  | Plus | Sharp (* level 2 *)
  | Dot | Slash | Semi | Question | Amper (* level 3 *)
  | Equal | Comma | Bang | At | Pipe (* reserved *)

type modifier =
  | Prefix of int
  | Explode

type 'a typ =
  | String : string typ
  | List : 'a typ -> 'a list typ
  | Kv : 'a typ -> (string * 'a) list typ

type 'a variable = { var : string ; modifier : modifier }

type expression = { op : operator ; vars : variable list }

(** Pretty printing *)

let char_of_op = function
  | No       -> None
  | Plus     -> Some '+'
  | Sharp    -> Some '#'
  | Dot      -> Some '.'
  | Slash    -> Some '/'
  | Semi     -> Some ';'
  | Question -> Some '?'
  | Amper    -> Some '&'
  | Equal    -> Some '='
  | Comma    -> Some ','
  | Bang     -> Some '!'
  | At       -> Some '@'
  | Pipe     -> Some '|'

let pp_op fmt op = Fmt.(option char) fmt (char_of_op op)

let pp_modifier fmt m = match m with
  | Explode -> Fmt.char fmt '*'
  | Prefix i -> Fmt.pf fmt ":%i" i

let pp_variable fmt { var ; modifier } =
  Fmt.pf fmt "%s%a" var  pp_modifier modifier

let pp fmt { op ; vars } =
  Fmt.pf fmt "{%a%a}"
    pp_op op
    Fmt.(list ~pp_sep:(const char ',') pp_variable) vars

(** Engine *)


let sep_of_op = function
  | No       -> ','
  | Plus     -> ','
  | Sharp    -> ','
  | Dot      -> '.'
  | Slash    -> '/'
  | Semi     -> ';'
  | Question -> '&'
  | Amper    -> '&'
  | Equal
  | Comma
  | Bang
  | At
  | Pipe     -> raise Not_implemented

let output ~encode env fmt { op ; vars } =
  let sep = sep_of_op op in
