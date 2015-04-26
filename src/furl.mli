(** Formatted Urls. *)

(** Bidirectional converters from one type to another. *)
module Converter : sig
  type ('a, 'b) t = {
    of_ : 'b -> 'a;
    to_ : 'a -> 'b
  }

  val id : ('a, 'a) t

end

(** An atom is an individual part of a path or a query.
    It represents a "hole" in the url.

    For example [Opt Int] is a hole of type [int option].

    Lists cannot be nested in anything.
    This is enforced by the type variable [`top].

    List are treated specially:
    - [List Int] in a path correspond to 4/5/6
    - [List Int] in a query correspond to 4,5,6

    To encode more complex datatypes, see the documentation for converters.
*)
type ('top,'a) atom =
  | Float  : (_, float) atom
  | Int    : (_, int) atom
  | Bool   : (_, bool) atom
  | String : (_, string) atom
  | Regexp : Re.t -> (_, string) atom
  | Opt    : ([`Notop], 'a) atom -> (_, 'a option) atom
  | Alt    :
      ([`Notop], 'a) atom * ([`Notop],'b) atom
    -> (_, [`Left of 'a | `Right of 'b]) atom
  | Seq    : ([`Notop], 'a) atom * ([`Notop], 'b) atom -> (_, 'a * 'b) atom
  | Prefix : string * ([`Notop], 'a) atom -> (_, 'a) atom
  | Suffix : ([`Notop], 'a) atom * string -> (_, 'a) atom
  | List   : ([`Notop], 'a) atom -> ([`Top], 'a list) atom
  | List1  : ([`Notop], 'a) atom -> ([`Top], 'a * 'a list) atom

(** {2 Query} *)

type ('fu, 'ret, 'converter, 'retc) query

val ( ** ) :
  string * ([`Top],'a) atom ->
  (      'c, 'b, 'e, 'd) query ->
  ('a -> 'c, 'b, 'e, 'd) query

val ( **! ) :
  string * ([`Top],'a) atom ->
  (      'c, 'd,                         'e, 'f) query ->
  ('b -> 'c, 'd, ('a, 'b) Converter.t -> 'e, 'f) query

val nil : ('a, 'a, 'b, 'b) query

val any : ('a, 'a, 'b, 'b) query

(** {2 Path} *)

type ('fu, 'return, 'converter, 'returnc) path

val host : string -> ('a, 'a, 'b, 'b) path
val rel : ('a, 'a, 'b, 'b) path
val (/) :
  ('b, 'a, 'd, 'c) path -> string ->
  ('b, 'a, 'd, 'c) path

val (/%) :
  ('c, 'a -> 'b, 'e, 'd) path -> ([`Top],'a) atom ->
  ('c,       'b, 'e, 'd) path

val (/!) :
  ('a, 'b -> 'c, 'd, ('e, 'b) Converter.t -> 'f) path -> ([`Top], 'e) atom ->
  ('a,       'c, 'd,                         'f) path

(** {2 Convertible Url} *)

type ('f, 'r, 'c, 'rc) url

val (/?) :
  ('b,     'a, 'd, 'c    ) path ->
  (    'a, 'e,     'c, 'f) query ->
  ('b,     'e, 'd,     'f) url

val (//?) :
  ('b, 'a,     'd, 'c    ) path ->
  (    'a, 'e,     'c, 'f) query ->
  ('b,     'e, 'd,     'f) url

(** {2 Base Url} *)

type ('r, 'f) furl

val finalize : ('f, 'r, 'c, ('f, 'r) furl) url -> 'c

val keval : ('a, 'b) furl -> (Uri.t -> 'b) -> 'a
val eval : ('a, Uri.t) furl -> 'a

val extract : ('f, 'r) furl -> f:'f -> Uri.t -> 'r

val get_re : ('f, 'r) furl -> Re.t
