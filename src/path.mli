
module Conv : sig
  type ('a, 'b) t = {
    of_ : 'b -> 'a;
    to_ : 'a -> 'b
  }

  val id : ('a, 'a) t

end

type ('a,'b) sum = L of 'a | R of 'b

type _ atom =
  | Float     : float atom
  | Int       : int atom
  | Int32     : int32 atom
  | Int64     : int64 atom
  | Nativeint : nativeint atom
  | Bool      : bool atom
  | String    : string atom
  | Opt       : 'a atom -> 'a option atom
  | Or        : 'a atom * 'b atom -> ('a,'b) sum atom
  | Star      : 'a atom -> 'a list atom
  | Plus      : 'a atom -> ('a * 'a list) atom
  | Seq       : 'a atom * 'b atom -> ('a * 'b) atom

(** {2 Query} *)

type ('ret, 'fu, 'retc, 'converter) query

val ( ** ) :
  string * 'a atom ->
  ('b,       'c, 'd, 'e) query ->
  ('b, 'a -> 'c, 'd, 'e) query

val ( **! ) :
  string * 'a atom ->
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
  ('a -> 'b, 'c, 'd, 'e) path -> 'a atom ->
  (      'b, 'c, 'd, 'e) path

val (/!) :
  ('a -> 'b, 'c, ('d, 'a) Conv.t -> 'e, 'f) path -> 'd atom ->
  (      'b, 'c,                    'e, 'f) path

(** {2 Convertible Uri} *)

type ('r, 'f, 'rc, 'c) conv_uri

val (/?) :
  (    'a, 'b,     'c, 'd) path ->
  ('e, 'a,     'f, 'c    ) query ->
  ('e,     'b, 'f,     'd) conv_uri

(** {2 Base Uri} *)

type ('r, 'f) uri

val finalize : ('r, 'f, ('r, 'f) uri, 'c) conv_uri -> 'c

val eval : (Uri.t, 'a) uri -> 'a
