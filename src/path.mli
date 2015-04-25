
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
  | Opt       : (nontop, 'a) atom -> (_, 'a option) atom
  | Or        : (nontop, 'a) atom * (nontop,'b) atom -> (nontop, ('a,'b) sum) atom
  | List      : (nontop, 'a) atom -> (top, 'a list) atom
  | List1     : (nontop, 'a) atom -> (top, 'a * 'a list) atom
  | Seq       : (nontop, 'a) atom * (nontop, 'b) atom -> (nontop, 'a * 'b) atom
  | Prefix    : string * (nontop, 'a) atom -> (nontop, 'a) atom
  | Suffix    : (nontop, 'a) atom * string -> (nontop, 'a) atom

(** {2 Query} *)

type ('ret, 'fu, 'retc, 'converter) query

val ( ** ) :
  string * (_,'a) atom ->
  ('b,       'c, 'd, 'e) query ->
  ('b, 'a -> 'c, 'd, 'e) query

val ( **! ) :
  string * (_,'a) atom ->
  ('b,       'c, 'd,                    'e) query ->
  ('b, 'f -> 'c, 'd, ('a, 'f) Conv.t -> 'e) query

val nil : ('a, 'a, 'b, 'b) query

(** {2 Path} *)

type ('return, 'fu, 'returnc, 'converter) path

val host : string -> ('a, 'a, 'b, 'b) path
val rel : ('a, 'a, 'b, 'b) path
val (/) :
  ('a, 'b, 'c, 'd) path -> string ->
  ('a, 'b, 'c, 'd) path

val (/%) :
  ('a -> 'b, 'c, 'd, 'e) path -> (_,'a) atom ->
  (      'b, 'c, 'd, 'e) path

val (/!) :
  ('a -> 'b, 'c, ('d, 'a) Conv.t -> 'e, 'f) path -> (_,'d) atom ->
  (      'b, 'c,                    'e, 'f) path

(** {2 Convertible Url} *)

type ('r, 'f, 'rc, 'c) conv_url

val (/?) :
  (    'a, 'b,     'c, 'd) path ->
  ('e, 'a,     'f, 'c    ) query ->
  ('e,     'b, 'f,     'd) conv_url

val (//?) :
  (    'a, 'b,     'c, 'd) path ->
  ('e, 'a,     'f, 'c    ) query ->
  ('e,     'b, 'f,     'd) conv_url

(** {2 Base Url} *)

type ('r, 'f) url

val finalize : ('r, 'f, ('r, 'f) url, 'c) conv_url -> 'c

val keval : ('a, 'b) url -> (Uri.t -> 'a) -> 'b
val eval : (Uri.t, 'a) url -> 'a
