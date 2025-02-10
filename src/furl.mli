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

(** Internal Types

    Please use the combinators.
*)
module Types : sig

  type ('fu, 'return) path =
    | Host : string -> ('r, 'r) path
    | Rel  : ('r, 'r) path
    | PathConst :
        ('f, 'r) path * string
     -> ('f, 'r) path
    | PathAtom :
        ('f,'a -> 'r) path * 'a Tyre.t
     -> ('f,      'r) path

  type ('fu, 'return) query =
    | Nil  : ('r,'r) query
    | Any  : ('r,'r) query
    | QueryAtom : string * 'a Tyre.t
        * (      'f, 'r) query
       -> ('a -> 'f, 'r) query

  type slash = Slash | NoSlash | MaybeSlash

  (** A convertible url is a path and a query (and potentially a slash).
      The type is the concatenation of both types.
  *)
  type ('f,'r) url =
    | Url : slash
        * ('f, 'x    ) path
        * (    'x, 'r) query
       -> ('f,     'r) url

end


(** {2 Path part of an url} *)
module Path : sig

  type ('fu, 'return) t =
    ('fu, 'return) Types.path

  [@@@pure]

  val host : string -> ('a, 'a) t
  (** [host "www.ocaml.org"] ≡ [//www.ocaml.org] *)

  val relative : ('a, 'a) t
  (** [relative] is the start of a relative url. *)

  val add :
    ('f,'r) t -> string ->
    ('f,'r) t
  (** [add path "foo"] ≡ [<path>/foo]. *)

  val add_atom :
    ('f,'a -> 'r) t -> 'a Tyre.t ->
    ('f,      'r) t
  (** [add_atom path atom] ≡ [<path>/<atom>].
      See the documentation of {!atom} for more details.
  *)

  val concat :
    ('f, 'x     ) t ->
    (    'x, 'r) t ->
    ('f,     'r) t
    (** [concat p1 p2] ≡ [<p1>/<p2>].
        If [p2] starts by a host name, the host is discarded.
    *)

end

(** {2 Query part of an url} *)
module Query : sig
  type ('fu, 'return) t =
    ('fu, 'return) Types.query

  [@@@pure]

  val nil : ('a, 'a) t
  (** The empty query. *)

  val any : ('a, 'a) t
  (** Any query parameter. *)

  val add :
    string -> 'a Tyre.t ->
    (      'f,'r) t ->
    ('a -> 'f,'r) t
  (** [add "myparam" atom query] ≡ [myparam=<atom>&<query>].
      See {!atom} documentation for more details.
  *)

  val concat :
    ('f, 'x    ) t ->
    (    'x, 'r) t ->
    ('f,     'r) t
    (** [concat q1 q2] ≡ [<q1>&<q2>]. *)

end

(** A complete url. *)
module Url : sig

  type ('f, 'r) t = ('f, 'r) Types.url

  type slash = Types.slash = Slash | NoSlash | MaybeSlash

  [@@@pure]

  val make :
    ?slash:slash ->
    ('f, 'x    ) Path.t ->
    (    'x, 'r) Query.t ->
    ('f,     'r) t

  (** [prefix_path p r] is the route formed by the concatenation of [p] and [r]. *)
  val prefix_path :
    ('a, 'b    ) Path.t ->
    (    'b, 'e) t ->
    ('a,     'e) t

  (** [add_query q r] is the route formed by the concatenation of [r] and [q]. *)
  val add_query :
    (    'a, 'b) Query.t ->
    ('e, 'a    ) t ->
    ('e,     'b) t

end

(** {2 Combinators} *)

[@@@pure]

val rel : ('r, 'r) Path.t

val host : string -> ('r, 'r) Path.t

val (/) :
  ('f,'r) Path.t -> string ->
  ('f,'r) Path.t

val (/%) :
  ('f,'a -> 'r) Path.t -> 'a Tyre.t ->
  ('f,      'r) Path.t

val nil : ('r, 'r) Query.t

val any : ('r, 'r) Query.t

val ( ** ) :
  string * 'a Tyre.t ->
  (      'f,'r) Query.t ->
  ('a -> 'f,'r) Query.t

val (/?) :
  ('f, 'x     ) Path.t ->
  (    'x, 'r) Query.t ->
  ('f,     'r) Url.t


val (//?) :
  ('f, 'x    ) Path.t ->
  (    'x, 'r) Query.t ->
  ('f,     'r) Url.t


val (/??) :
  ('f, 'x    ) Path.t ->
  (    'x, 'r) Query.t ->
  ('f,     'r) Url.t


(** An url with an empty list of converters.
    It can be evaluated/extracted/matched against.
 *)
type ('f, 'r) t = ('f, 'r) Url.t

[@@@impure]

val keval : ('f, 'r) t -> (Uri.t -> 'r) -> 'f
val eval : ('f, Uri.t) t -> 'f

val extract : ('f, 'r) t -> f:'f -> Uri.t -> 'r

val get_re : _ t -> Re.t

type 'r route
val route : ('f, 'r) t -> 'f -> 'r route [@@pure]

val (-->) : ('f, 'r) t -> 'f -> 'r route [@@pure]

val match_url : default:(Uri.t -> 'r) -> 'r route list -> Uri.t -> 'r


(** {2 Utils} *)

val (~$) : (unit -> 'a) -> 'a [@@pure]
(** [~$f] ≡ [f()]. Often allow to remove parens. *)
