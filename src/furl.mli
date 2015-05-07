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


(** Internal Types

    Please use the combinators.
*)
module Types : sig

  type ('fu, 'return, 'converter, 'returnc) path_ty =
    | Host : string -> ('r, 'r, 'rc, 'rc) path_ty
    | Rel  : ('r, 'r ,'rc, 'rc) path_ty
    | SuffixConst :
        ('f, 'r, 'c, 'rc) path_ty * string
      -> ('f, 'r, 'c, 'rc) path_ty
    | SuffixAtom :
        ('f,'a -> 'r, 'c, 'rc) path_ty * ([`Top],'a) atom
      -> ('f,      'r, 'c, 'rc) path_ty
    | SuffixConv :
        ('f, 'b -> 'r, 'c, ('a, 'b) Converter.t -> 'rc) path_ty
        * ([`Top],'a) atom
      -> ('f,       'r, 'c,                         'rc) path_ty

  type ('fu, 'return, 'converter, 'returnc) query_ty =
    | Nil  : ('r,'r, 'rc,'rc) query_ty
    | Any  : ('r,'r, 'rc,'rc) query_ty

    | Cons : string * ([`Top],'a) atom
        * (      'f, 'r, 'c, 'rc) query_ty
      -> ('a -> 'f, 'r, 'c, 'rc) query_ty

    | Conv : string * ([`Top],'a) atom
        * (      'f, 'r,                         'c, 'rc) query_ty
      -> ('b -> 'f, 'r, ('a, 'b) Converter.t -> 'c, 'rc) query_ty

  type slash = Slash | NoSlash | MaybeSlash

  (** A url is a path and a query (and potentially a slash).
      The type is the concatenation of both types.
  *)
  type ('f,'r,'c, 'rc) url_ty =
    | ConvUrl : slash
        * ('f, 'x,     'c, 'xc     ) path_ty
        * (    'x, 'r,     'xc, 'rc) query_ty
      -> ('f,     'r, 'c,      'rc) url_ty

end


type slash = Types.slash = Slash | NoSlash | MaybeSlash

(** {2 Path part of an url} *)
module Path : sig

  type ('fu, 'return, 'converter, 'returnc) t =
    ('fu, 'return, 'converter, 'returnc) Types.path_ty

  val host : string -> ('a, 'a, 'b, 'b) t
  (** [host "www.ocaml.org"] ≡ [//www.ocaml.org] *)

  val relative : ('a, 'a, 'b, 'b) t
  (** [relative] is the start of a relative url. *)

  val add :
    ('a, 'b, 'c, 'd) t ->
    string -> ('a, 'b, 'c, 'd) t
  (** [add path "foo"] ≡ [<path>/foo]. *)

  val add_atom :
    ('a, 'b -> 'c, 'd, 'e) t ->
    ([ `Top ], 'b) atom -> ('a, 'c, 'd, 'e) t
  (** [add_atom path atom] ≡ [<path>/<atom>].
      See the documentation of {!atom} for more details.
  *)

  val add_conv :
    ('a, 'b -> 'c, 'd, ('e, 'b) Converter.t -> 'f) t ->
    ([ `Top ], 'e) atom -> ('a, 'c, 'd, 'f) t
  (** Similar to [add_atom], but also add the atom to the list of converters.
      See the documentation {!finalize} for more details.
  *)

  val concat :
    ('f, 'x, 'c, 'xc) t -> ('x, 'r, 'xc, 'rc) t -> ('f, 'r, 'c, 'rc) t
    (** [concat p1 p2] ≡ [<p1>/<p2>].
        If [p2] starts by a host name, the host is discarded.
    *)

end

(** {2 Query part of an url} *)
module Query : sig
  type ('fu, 'return, 'converter, 'returnc) t =
    ('fu, 'return, 'converter, 'returnc) Types.query_ty

  val nil : ('a, 'a, 'b, 'b) t
  (** The empty query. *)

  val any : ('a, 'a, 'b, 'b) t
  (** Any query parameter. *)

  val add :
    string ->
    ([ `Top ], 'a) atom ->
    ('b, 'c, 'd, 'e) t ->
    ('a -> 'b, 'c, 'd, 'e) t
  (** [add "myparam" atom query] ≡ [myparam=<atom>&<query>].
      See {!atom} documentation for more details.
  *)

  val add_conv :
    string ->
    ([ `Top ], 'a) atom ->
    ('b, 'c, 'd, 'e) t ->
    ('f -> 'b, 'c, ('a, 'f) Converter.t -> 'd, 'e) t
  (** Similar to [add], but also add the atom to the list of converters.
      See the documentation of {!finalize} for more details.
  *)

  val concat :
    ('f, 'x, 'c, 'xc) t -> ('x, 'r, 'xc, 'rc) t -> ('f, 'r, 'c, 'rc) t
    (** [concat q1 q2] ≡ [<q1>&<q2>]. *)

end


(** {2 Combinators} *)
val rel : ('r, 'r, 'rc, 'rc) Path.t

val host : string -> ('r, 'r, 'rc, 'rc) Path.t

val (/) :
  ('f,'r,'c,'rc) Path.t -> string ->
  ('f,'r,'c,'rc) Path.t

val (/%) :
  ('f,'a -> 'r,'c,'rc) Path.t -> ([ `Top ], 'a) atom ->
  ('f,      'r,'c,'rc) Path.t

val (/!) :
  ('f,'b -> 'r,'c,('a, 'b) Converter.t -> 'rc) Path.t -> ([ `Top ], 'a) atom ->
  ('f,      'r,'c,                        'rc) Path.t

val nil : ('r, 'r, 'rc, 'rc) Query.t

val any : ('r, 'r, 'rc, 'rc) Query.t

val ( ** ) :
  string * ([ `Top ], 'a) atom ->
  (      'f,'r,'c,'rc) Query.t ->
  ('a -> 'f,'r,'c,'rc) Query.t

val ( **! ) :
  string * ([ `Top ], 'a) atom ->
  (      'f,'r,                        'c,'rc) Query.t ->
  ('b -> 'f,'r,('a, 'b) Converter.t -> 'c,'rc) Query.t


(** {2 Convertible Url} *)

type ('fu, 'return, 'converter, 'returnc) url =
  ('fu, 'return, 'converter, 'returnc) Types.url_ty

type ('fu, 'return) furl

val (/?) :
  ('a, 'b,     'c, 'd    ) Path.t ->
  (    'b, 'e,     'd, 'f) Query.t ->
  ('a,     'e, 'c,     'f) url

val (//?) :
  ('a, 'b,     'c, 'd    ) Path.t ->
  (    'b, 'e,     'd, 'f) Query.t ->
  ('a,     'e, 'c,     'f) url

val (/??) :
  ('a, 'b,     'c, 'd    ) Path.t ->
  (    'b, 'e,     'd, 'f) Query.t ->
  ('a,     'e, 'c,     'f) url

(** {2 Base Url} *)


val finalize : ('f, 'r, 'c, ('f, 'r) furl) url -> 'c

val keval : ('a, 'b) furl -> (Uri.t -> 'b) -> 'a
val eval : ('a, Uri.t) furl -> 'a

val extract : ('f, 'r) furl -> f:'f -> Uri.t -> 'r

val get_re : ('f, 'r) furl -> Re.t

type 'r ex
val ex : ('f, 'r) furl -> 'f -> 'r ex

val match_url : default:(Uri.t -> 'r) -> 'r ex list -> Uri.t -> 'r


(** {2 Utils} *)

val (~$) : (unit -> 'a) -> 'a
(** [~$f] ≡ [f()]. Often allow to remove parens. *)
