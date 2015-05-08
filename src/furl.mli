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

  type ('fu, 'return, 'converter, 'returnc) path_ty =
    | Host : string -> ('r, 'r, 'rc, 'rc) path_ty
    | Rel  : ('r, 'r ,'rc, 'rc) path_ty
    | PathConst :
        ('f, 'r, 'c, 'rc) path_ty * string
     -> ('f, 'r, 'c, 'rc) path_ty
    | PathAtom :
        ('f,'a -> 'r, 'c, 'rc) path_ty * ([`Top],'a) atom
     -> ('f,      'r, 'c, 'rc) path_ty
    | PathConv :
        ('f, 'b -> 'r, 'c, ('a, 'b) Converter.t -> 'rc) path_ty
        * ([`Top],'a) atom
     -> ('f,       'r, 'c,                         'rc) path_ty

  type ('fu, 'return, 'converter, 'returnc) query_ty =
    | Nil  : ('r,'r, 'rc,'rc) query_ty
    | Any  : ('r,'r, 'rc,'rc) query_ty

    | QueryAtom : string * ([`Top],'a) atom
        * (      'f, 'r, 'c, 'rc) query_ty
       -> ('a -> 'f, 'r, 'c, 'rc) query_ty

    | QueryConv : string * ([`Top],'a) atom
        * (      'f, 'r,                         'c, 'rc) query_ty
       -> ('b -> 'f, 'r, ('a, 'b) Converter.t -> 'c, 'rc) query_ty

  type slash = Slash | NoSlash | MaybeSlash

  (** A url is a path and a query (and potentially a slash).
      The type is the concatenation of both types.
  *)
  type ('f,'r,'c, 'rc) convurl =
    | Url : slash
       * ('f, 'x,     'c, 'xc     ) path_ty
       * (    'x, 'r,     'xc, 'rc) query_ty
      -> ('f,     'r, 'c,      'rc) convurl


end


(** {2 Path part of an url} *)
module Path : sig

  type ('fu, 'return, 'converter, 'returnc) t =
    ('fu, 'return, 'converter, 'returnc) Types.path_ty

  val host : string -> ('a, 'a, 'b, 'b) t
  (** [host "www.ocaml.org"] ≡ [//www.ocaml.org] *)

  val relative : ('a, 'a, 'b, 'b) t
  (** [relative] is the start of a relative url. *)

  val add :
    ('f,'r,'c,'rc) t -> string ->
    ('f,'r,'c,'rc) t
  (** [add path "foo"] ≡ [<path>/foo]. *)

  val add_atom :
    ('f,'a -> 'r,'c,'rc) t -> ([ `Top ], 'a) atom ->
    ('f,      'r,'c,'rc) t
  (** [add_atom path atom] ≡ [<path>/<atom>].
      See the documentation of {!atom} for more details.
  *)

  val add_conv :
    ('f,'b -> 'r,'c,('a, 'b) Converter.t -> 'rc) t -> ([ `Top ], 'a) atom ->
    ('f,      'r,'c,                        'rc) t
  (** Similar to [add_atom], but also add the atom to the list of converters.
      See the documentation {!finalize} for more details.
  *)

  val concat :
    ('f, 'x,     'c, 'xc     ) t ->
    (    'x, 'r,     'xc, 'rc) t ->
    ('f,     'r, 'c,      'rc) t
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
    string -> ([ `Top ], 'a) atom ->
    (      'f,'r,'c,'rc) t ->
    ('a -> 'f,'r,'c,'rc) t
  (** [add "myparam" atom query] ≡ [myparam=<atom>&<query>].
      See {!atom} documentation for more details.
  *)

  val add_conv :
    string -> ([ `Top ], 'a) atom ->
    (      'f,'r,                        'c,'rc) t ->
    ('b -> 'f,'r,('a, 'b) Converter.t -> 'c,'rc) t
  (** Similar to [add], but also add the atom to the list of converters.
      See the documentation of {!finalize} for more details.
  *)

  val concat :
    ('f, 'x,     'c, 'xc     ) t ->
    (    'x, 'r,     'xc, 'rc) t ->
    ('f,     'r, 'c,      'rc) t
    (** [concat q1 q2] ≡ [<q1>&<q2>]. *)

end

(** A complete convertible Url. *)
module Url : sig

  type ('f, 'r, 'final, 'c, 'rc) t

  type slash = Types.slash = Slash | NoSlash | MaybeSlash

  val make :
    ?slash:slash ->
    ('f, 'x,              'c, 'xc     ) Path.t ->
    (    'x, 'r,              'xc, 'rc) Query.t ->
    ('f,     'r, [`NotF], 'c,      'rc) t

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

val (/?) :
  ('f, 'x,              'c, 'xc     ) Path.t ->
  (    'x, 'r,              'xc, 'rc) Query.t ->
  ('f,     'r, [`NotF], 'c,      'rc) Url.t


val (//?) :
  ('f, 'x,              'c, 'xc     ) Path.t ->
  (    'x, 'r,              'xc, 'rc) Query.t ->
  ('f,     'r, [`NotF], 'c,      'rc) Url.t


val (/??) :
  ('f, 'x,              'c, 'xc     ) Path.t ->
  (    'x, 'r,              'xc, 'rc) Query.t ->
  ('f,     'r, [`NotF], 'c,      'rc) Url.t


(** An url with an empty list of converters.
    It can be evaluated/extracted/matched against.
 *)
type ('f, 'r, 'd, 'rc) t =
  ('f, 'r, 'd,'rc,'rc) Url.t
  constraint 'rc = _ Url.t

val finalize :
  ('f, 'r, [`NotF], 'c, ('f, 'r, [`F], _) t) Url.t -> 'c

val keval : ('f, 'r, _, _) t -> (Uri.t -> 'r) -> 'f
val eval : ('f, Uri.t, _, _) t -> 'f

val extract : ('f, 'r, _, _) t -> f:'f -> Uri.t -> 'r

val get_re : _ t -> Re.t

type 'r route
val route : ('f, 'r, _, _) t -> 'f -> 'r route

val (-->) : ('f, 'r, _, _) t -> 'f -> 'r route

val match_url : default:(Uri.t -> 'r) -> 'r route list -> Uri.t -> 'r


(** {2 Utils} *)

val (~$) : (unit -> 'a) -> 'a
(** [~$f] ≡ [f()]. Often allow to remove parens. *)
