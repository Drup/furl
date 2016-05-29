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

    Lists cannot only be at "toplevel" in an atom.
    This is enforced by the type variable [`top].

    List are treated specially:
    - [List Int] in a path correspond to 4/5/6
    - [List Int] in a query correspond to 4,5,6

    To encode more complex datatypes, see the documentation of {!finalize}.
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

  type ('fu, 'return) path_ty =
    | Host : string -> ('r, 'r) path_ty
    | Rel  : ('r, 'r) path_ty
    | PathConst :
        ('f, 'r) path_ty * string
     -> ('f, 'r) path_ty
    | PathAtom :
        ('f,'a -> 'r) path_ty * ([`Top],'a) atom
     -> ('f,      'r) path_ty
    | PathConv :
        ('f, 'b -> 'r) path_ty
        * ([`Top],'a) atom * ('a, 'b) Converter.t
     -> ('f,       'r) path_ty

  type ('fu, 'return) query_ty =
    | Nil  : ('r,'r) query_ty
    | Any  : ('r,'r) query_ty
    | QueryAtom : string * ([`Top],'a) atom
        * (      'f, 'r) query_ty
       -> ('a -> 'f, 'r) query_ty

    | QueryConv : string * ([`Top],'a) atom * ('a, 'b) Converter.t
        * (      'f, 'r) query_ty
       -> ('b -> 'f, 'r) query_ty

  type slash = Slash | NoSlash | MaybeSlash

  (** A convertible url is a path and a query (and potentially a slash).
      The type is the concatenation of both types.
  *)
  type ('f,'r) url_ty =
    | Url : slash
        * ('f, 'x    ) path_ty
        * (    'x, 'r) query_ty
       -> ('f,     'r) url_ty

end


(** {2 Path part of an url} *)
module Path : sig

  type ('fu, 'return) t =
    ('fu, 'return) Types.path_ty

  val host : string -> ('a, 'a) t
  (** [host "www.ocaml.org"] ≡ [//www.ocaml.org] *)

  val relative : ('a, 'a) t
  (** [relative] is the start of a relative url. *)

  val add :
    ('f,'r) t -> string ->
    ('f,'r) t
  (** [add path "foo"] ≡ [<path>/foo]. *)

  val add_atom :
    ('f,'a -> 'r) t -> ([ `Top ], 'a) atom ->
    ('f,      'r) t
  (** [add_atom path atom] ≡ [<path>/<atom>].
      See the documentation of {!atom} for more details.
  *)

  val add_conv :
    ('f,'b -> 'r) t -> ([ `Top ], 'a) atom -> ('a, 'b) Converter.t ->
    ('f,      'r) t
  (** Similar to [add_atom], but also add the atom to the list of converters.
      See the documentation {!finalize} for more details.
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
    ('fu, 'return) Types.query_ty

  val nil : ('a, 'a) t
  (** The empty query. *)

  val any : ('a, 'a) t
  (** Any query parameter. *)

  val add :
    string -> ([ `Top ], 'a) atom ->
    (      'f,'r) t ->
    ('a -> 'f,'r) t
  (** [add "myparam" atom query] ≡ [myparam=<atom>&<query>].
      See {!atom} documentation for more details.
  *)

  val add_conv :
    string -> ([ `Top ], 'a) atom -> ('a, 'b) Converter.t ->
    (      'f,'r) t ->
    ('b -> 'f,'r) t
  (** Similar to [add], but also add the atom to the list of converters.
      See the documentation of {!finalize} for more details.
  *)

  val concat :
    ('f, 'x    ) t ->
    (    'x, 'r) t ->
    ('f,     'r) t
    (** [concat q1 q2] ≡ [<q1>&<q2>]. *)

end

(** A complete convertible Url. *)
module Url : sig

  type ('f, 'r) t

  type slash = Types.slash = Slash | NoSlash | MaybeSlash

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

val rel : ('r, 'r) Path.t

val host : string -> ('r, 'r) Path.t

val (/) :
  ('f,'r) Path.t -> string ->
  ('f,'r) Path.t

val (/%) :
  ('f,'a -> 'r) Path.t -> ([ `Top ], 'a) atom ->
  ('f,      'r) Path.t

val (/!) :
  ('f,'b -> 'r) Path.t -> (([ `Top ], 'a) atom * ('a, 'b) Converter.t) ->
  ('f,      'r) Path.t

val nil : ('r, 'r) Query.t

val any : ('r, 'r) Query.t

val ( ** ) :
  string * ([ `Top ], 'a) atom ->
  (      'f,'r) Query.t ->
  ('a -> 'f,'r) Query.t

val ( **! ) :
  string * ([ `Top ], 'a) atom * ('a, 'b) Converter.t ->
  (      'f,'r) Query.t ->
  ('b -> 'f,'r) Query.t

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

val keval : ('f, 'r) t -> (Uri.t -> 'r) -> 'f
val eval : ('f, Uri.t) t -> 'f

val extract : ('f, 'r) t -> f:'f -> Uri.t -> 'r

val get_re : _ t -> Re.t

type 'r route
val route : ('f, 'r) t -> 'f -> 'r route

val (-->) : ('f, 'r) t -> 'f -> 'r route

val match_url : default:(Uri.t -> 'r) -> 'r route list -> Uri.t -> 'r


(** {2 Utils} *)

val (~$) : (unit -> 'a) -> 'a
(** [~$f] ≡ [f()]. Often allow to remove parens. *)
