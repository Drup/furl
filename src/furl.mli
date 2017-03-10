(** Formatted Urls. *)

(**
   This library allows to build formatted url in the style of
   the [Printf], [Format] and [Scanf] modules.

   This can be used both for client and servers.
*)

(** An atom is an individual part of a path or a query.
    It represents a "hole" in the url.

    An atom can be any regular expression encoded using {!Tyre}.
    For example [Tyre.(opt int)] is a hole of type [int option].

    Repetition at toplevel of an atom is treated specially:
    - [Tyre.(list int)] in a path correspond to 4/5/6
    - [Tyre.(list int)] in a query correspond to 4,5,6
*)
type 'a atom = 'a Tyre.t

(* (\** Internal Types *)

(*     Please use the combinators. *)
(* *\) *)
(* module Types : sig *)

(*   type ('fu, 'return) path = *)
(*     | Host : string -> ('r, 'r) path *)
(*     | Rel  : ('r, 'r) path *)
(*     | PathConst : *)
(*         ('f, 'r) path * string *)
(*      -> ('f, 'r) path *)
(*     | PathAtom : *)
(*         ('f,'a -> 'r) path * 'a Tyre.t *)
(*      -> ('f,      'r) path *)

(*   type ('fu, 'return) query = *)
(*     | Nil  : ('r,'r) query *)
(*     | Any  : ('r,'r) query *)
(*     | QueryAtom : string * 'a Tyre.t *)
(*         * (      'f, 'r) query *)
(*        -> ('a -> 'f, 'r) query *)

(*   type slash = Slash | NoSlash | MaybeSlash *)

(*   (\** A convertible url is a path and a query (and potentially a slash). *)
(*       The type is the concatenation of both types. *)
(*   *\) *)
(*   type ('f,'r) url = *)
(*     | Url : slash *)
(*         * ('f, 'x    ) path *)
(*         * (    'x, 'r) query *)
(*        -> ('f,     'r) url *)

(* end *)


type ('f, 'r) t

val rel : ('r, 'r) t

val host : string -> ('r, 'r) t

val (/) :
  ('f,'r) t -> string ->
  ('f,'r) t

val (/%) :
  ('f,'a -> 'r) t -> 'a Tyre.t ->
  ('f,      'r) t

val (/?) :
  ('f,'a -> 'r) t -> string * 'a Tyre.t ->
  ('f,      'r) t

val concat :
  ('f, 'x    ) t ->
  (    'x, 'r) t ->
  ('f,     'r) t

val keval : ('f, 'r) t -> (Uri.t -> 'r) -> 'f
val eval : ('f, Uri.t) t -> 'f

val extract : ('f, 'r) t -> f:'f -> Uri.t -> 'r

(* val get_re : _ t -> Re.t *)

type 'r route
val route : ('f, 'r) t -> 'f -> 'r route

val (-->) : ('f, 'r) t -> 'f -> 'r route

val match_url : default:(Uri.t -> 'r) -> 'r route list -> Uri.t -> 'r


(** {2 Utils} *)

val (~$) : (unit -> 'a) -> 'a
(** [~$f] ≡ [f()]. Often allow to remove parens. *)
