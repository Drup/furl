let slash = Re.char '/'
let comma = Re.char ','
let amper = Re.char '&'

(** -?[0-9]+( .[0-9]* )? *)
let float =
  let open Re in
  seq [opt (char '-') ; rep1 digit ; opt (seq [char '.'; rep digit])]

(** -?[0-9]+ *)
let arbitrary_int =
  let open Re in
  seq [opt (char '-') ; rep1 digit]

(** true|false *)
let bool =
  let open Re in
  alt [str "true" ; str "false"]

(** Non empty list of safe chars *)
(* Should use Uri's safe chars *)
let string component =
  let open Re in match component with
    | `Path -> rep1 @@ compl [slash]
    | `Query_value -> rep1 @@ compl [set "&;+,"]

(** Separated by , or by / *)
let list ?m ~component n re =
  let open Re in match component with
    | `Path -> repn (seq [slash; re]) n m
    | `Query_value ->
      if n = 0 then
        alt [ epsilon ; seq [ re ; repn (seq [comma; re]) 0 m ] ]
      else
        seq [re ; repn (seq [comma; re]) (n-1) m]

let query_sep ~any =
  if not any then amper
  else
    let open Re in
    seq [amper; rep @@ seq [rep1 @@ compl [amper] ; amper]]
