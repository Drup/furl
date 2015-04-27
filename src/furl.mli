(** Formatted Urls. *)

(**
   This library allows to build formatted url in the style of
   the [Printf], [Format] and [Scanf] modules.

   This can be used both for client and servers.
*)

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

    To encode more complex datatypes, see the documentation for converters
    and the examples.
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

(** {2 Path} *)

type ('fu, 'return, 'converter, 'returnc) path =
  | Host : string -> ('r, 'r, 'rc, 'rc) path
  | Rel  : ('r, 'r ,'rc, 'rc) path
  | SuffixConst :
      ('f, 'r, 'c, 'rc) path * string
    -> ('f, 'r, 'c, 'rc) path
  | SuffixAtom :
      ('f,'a -> 'r, 'c, 'rc) path * ([`Top],'a) atom
    -> ('f,      'r, 'c, 'rc) path
  | SuffixConv :
      ('f, 'b -> 'r, 'c, ('a, 'b) Converter.t -> 'rc) path
      * ([`Top],'a) atom
    -> ('f,       'r, 'c,                         'rc) path

val (/) :
  ('b, 'a, 'd, 'c) path -> string ->
  ('b, 'a, 'd, 'c) path

val (/%) :
  ('c, 'a -> 'b, 'e, 'd) path -> ([`Top],'a) atom ->
  ('c,       'b, 'e, 'd) path

val (/!) :
  ('a, 'b -> 'c, 'd, ('e, 'b) Converter.t -> 'f) path -> ([`Top], 'e) atom ->
  ('a,       'c, 'd,                         'f) path

(** {2 Query} *)

type ('fu, 'return, 'converter, 'returnc) query =
  | Nil  : ('r,'r, 'rc,'rc) query
  | Any  : ('r,'r, 'rc,'rc) query

  | Cons : string * ([`Top],'a) atom
      * (      'f, 'r, 'c, 'rc) query
    -> ('a -> 'f, 'r, 'c, 'rc) query

  | Conv : string * ([`Top],'a) atom
      * (      'f, 'r,                         'c, 'rc) query
    -> ('b -> 'f, 'r, ('a, 'b) Converter.t -> 'c, 'rc) query

val ( ** ) :
  string * ([`Top],'a) atom ->
  (      'c, 'b, 'e, 'd) query ->
  ('a -> 'c, 'b, 'e, 'd) query

val ( **! ) :
  string * ([`Top],'a) atom ->
  (      'c, 'd,                         'e, 'f) query ->
  ('b -> 'c, 'd, ('a, 'b) Converter.t -> 'e, 'f) query


(** {2 Convertible Url} *)

type ('f, 'r, 'c) url

type ('f, 'r) furl

val (/?) :
  ('f, 'x,     'c, 'xc               ) path ->
  (    'x, 'r,     'xc, ('f, 'r) furl) query ->
  ('f,     'r, 'c) url

val (//?) :
  ('f, 'x,     'c, 'xc               ) path ->
  (    'x, 'r,     'xc, ('f, 'r) furl) query ->
  ('f,     'r, 'c) url

(** {2 Base Url} *)


val finalize : ('f, 'r, 'c) url -> 'c

val keval : ('a, 'b) furl -> (Uri.t -> 'b) -> 'a
val eval : ('a, Uri.t) furl -> 'a

val extract : ('f, 'r) furl -> f:'f -> Uri.t -> 'r

val get_re : ('f, 'r) furl -> Re.t

type 'r ex
val ex : ('f, 'r) furl -> 'f -> 'r ex

val match_url : default:(Uri.t -> 'r) -> 'r ex list -> Uri.t -> 'r
