
module Conv : sig
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
  | Float     : (nontop, float) atom
  | Int       : (nontop, int) atom
  | Bool      : (nontop, bool) atom
  | String    : (nontop, string) atom
  | Regexp    : Re.t -> (nontop, string) atom
  | Opt       : (nontop, 'a) atom -> (_, 'a option) atom
  | Or        : (nontop, 'a) atom * (nontop,'b) atom -> (nontop, ('a,'b) sum) atom
  | Seq       : (nontop, 'a) atom * (nontop, 'b) atom -> (nontop, 'a * 'b) atom
  | Prefix    : string * (nontop, 'a) atom -> (nontop, 'a) atom
  | Suffix    : (nontop, 'a) atom * string -> (nontop, 'a) atom
  | List      : (nontop, 'a) atom -> (top, 'a list) atom
  | List1     : (nontop, 'a) atom -> (top, 'a * 'a list) atom

(** {2 Query} *)

type ('fu, 'ret, 'converter, 'retc) query

val ( ** ) :
  string * (_,'a) atom ->
  (      'c, 'b, 'e, 'd) query ->
  ('a -> 'c, 'b, 'e, 'd) query

val ( **! ) :
  string * (_,'a) atom ->
  (      'c, 'd,                    'e, 'f) query ->
  ('b -> 'c, 'd, ('a, 'b) Conv.t -> 'e, 'f) query

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
  ('c, 'a -> 'b, 'e, 'd) path -> (_,'a) atom ->
  ('c,       'b, 'e, 'd) path

val (/!) :
  ('a, 'b -> 'c, 'd, ('e, 'b) Conv.t -> 'f) path -> ('g, 'e) atom ->
  ('a,       'c, 'd,                    'f) path

(** {2 Convertible Url} *)

type ('f, 'r, 'c, 'rc) conv_url

val (/?) :
  ('b,     'a, 'd, 'c    ) path ->
  (    'a, 'e,     'c, 'f) query ->
  ('b,     'e, 'd,     'f) conv_url

val (//?) :
  ('b, 'a,     'd, 'c    ) path ->
  (    'a, 'e,     'c, 'f) query ->
  ('b,     'e, 'd,     'f) conv_url

(** {2 Base Url} *)

type ('r, 'f) url

val finalize : ('f, 'r, 'c, ('f, 'r) url) conv_url -> 'c

val keval : ('a, 'b) url -> (Uri.t -> 'b) -> 'a
val eval : ('a, Uri.t) url -> 'a

val extract : ('f, 'r) url -> f:'f -> Uri.t -> 'r

val get_re : ('f, 'r) url -> Re.t
