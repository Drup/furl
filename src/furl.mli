(** Formatted Urls. *)

(** Bidirectional converters from one type to another. *)
module Converter : sig
  type ('a, 'b) t = {
    of_ : 'b -> 'a;
    to_ : 'a -> 'b
  }

  val id : ('a, 'a) t

end

type ('a,'b) sum = L of 'a | R of 'b

type top = Top
type nontop = NonTop

type (_,_) atom =
  | Float  : (_, float) atom
  | Int    : (_, int) atom
  | Bool   : (_, bool) atom
  | String : (_, string) atom
  | Regexp : Re.t -> (_, string) atom
  | Opt    : (nontop, 'a) atom -> (_, 'a option) atom
  | Or     :
      (nontop, 'a) atom * (nontop,'b) atom -> (_, ('a,'b) sum) atom
  | Seq    : (nontop, 'a) atom * (nontop, 'b) atom -> (_, 'a * 'b) atom
  | Prefix : string * (nontop, 'a) atom -> (_, 'a) atom
  | Suffix : (nontop, 'a) atom * string -> (_, 'a) atom
  | List   : (nontop, 'a) atom -> (top, 'a list) atom
  | List1  : (nontop, 'a) atom -> (top, 'a * 'a list) atom

(** {2 Query} *)

type ('fu, 'ret, 'converter, 'retc) query

val ( ** ) :
  string * (top,'a) atom ->
  (      'c, 'b, 'e, 'd) query ->
  ('a -> 'c, 'b, 'e, 'd) query

val ( **! ) :
  string * (top,'a) atom ->
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
  ('c, 'a -> 'b, 'e, 'd) path -> (top,'a) atom ->
  ('c,       'b, 'e, 'd) path

val (/!) :
  ('a, 'b -> 'c, 'd, ('e, 'b) Converter.t -> 'f) path -> (top, 'e) atom ->
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
