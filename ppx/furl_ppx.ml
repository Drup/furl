
module A = struct
  include Ast_helper
  include Ast_mapper
  include Ast_convenience
end

module L = Location

open Parsetree

(** Utils **)

let exp_errorf ?loc ?sub ?if_highlight =
  let k msg =
    A.Exp.extension ?loc (A.extension_of_error @@
        L.error ?loc ?sub ?if_highlight msg
    )
  in Printf.ksprintf k

let pat_errorf ?loc ?sub ?if_highlight =
  let k msg =
    A.Pat.extension ?loc (A.extension_of_error @@
        L.error ?loc ?sub ?if_highlight msg
    )
  in Printf.ksprintf k

(** Body used in erroneous branches in url matching. *)
let dummy_body = A.unit ()

let is_catchall = function
  | Ppat_var _
  | Ppat_any ->
    true
  | _ -> false

(** Url creation **)

let furl_string loc str =
  assert false

(** Url matching **)

let rec extract_routes routes catchall errors = function
  | [] -> List.rev routes, List.rev catchall, List.rev errors
  | { pc_guard = Some expr } :: t ->
    let error =
      exp_errorf ~loc:expr.pexp_loc
        "Furl: Guards are not allowed in url matching."
    in
    extract_routes routes catchall (error::errors) t
  | { pc_lhs=
        {ppat_desc =
           Ppat_constant (Const_string (str, (Some ("furl"|"") | None))) ;
         ppat_loc } ;
      pc_rhs ;
    } :: t
    -> extract_routes ((ppat_loc, str, pc_rhs) :: routes) catchall errors t
  | { pc_lhs=pat; pc_rhs} :: t when is_catchall pat.ppat_desc ->
    extract_routes routes ((pat, pc_rhs)::catchall) errors t
  | { pc_lhs= pat } :: t ->
    let error =
      exp_errorf ~loc:pat.ppat_loc
        "Furl: This kind of pattern is not valid in a url matching."
    in
    extract_routes routes catchall (error::errors) t

let create_matchings loc routes =
  A.default_loc := loc ;
  List.map (fun (loc, url, body) ->
    [%expr Furl.route [%e furl_string loc url] [%e body]])
    routes

let create_furl_matching loc l =
  let routes, catchalls, errors = extract_routes [] [] [] l in
  let pat_default, body_default = match catchalls with
    | [] ->
      pat_errorf ~loc
        "Furl: This url matching contains no default route."
      , dummy_body
    | [ x ] -> x
    | l ->
      let sub = l |> List.map @@ fun (pat,_) ->
        L.error ~loc:pat.ppat_loc
          "Furl: This catchall route is superfluous."
      in
      pat_errorf ~loc ~sub
        "Furl: This url matching contains more than one default route."
      , dummy_body
  in
  let routes = create_matchings loc routes in
  let routes = if errors = [] then routes else routes @ errors in
  [%expr
    Furl.match_url
      ~default:(fun [%p pat_default] -> [%e body_default])
      [%e A.list routes]
  ]


let furl_mapper _args =
  {Ast_mapper.default_mapper with
     expr = (fun mapper expr ->
       let loc = expr.pexp_loc in
       match expr with
       | {pexp_desc = Pexp_constant (Const_string (str, Some "furl")) } ->
         furl_string loc str
       | [%expr [%furl [%e? {pexp_desc = Pexp_function l} ]]] ->
         create_furl_matching loc l
       | expr -> Ast_mapper.default_mapper.expr mapper expr
     )
  }
