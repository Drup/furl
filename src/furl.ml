open Furl_utils

(**
   {2 Meta-variables}

   To better read this file, here is the method of variable naming:

   For type variables:
   - f is a function type
   - r is a return type of the associated f
   - c are for types related to converter, used with r or f
   - x is the intermediate type for type-level concatenation of diff-list.

   For variables:
   - re, r are for regular expressions
   - id is Re's markid
   - a is for atom
   - p is for path
   - q is for query
   - cl is for converter list
   - w is for witness (see section matching)
   - k is for kontinuation (with a k).

   Merlin is heavily recommended to browse this code.
*)

module Converter = struct

  type ('a, 'b) t = {
    of_ : 'b -> 'a;
    to_ : 'a -> 'b
  }

  let id =
    let id x = x in
    { of_ = id ; to_ = id }

end

(** {2 Pseudo-lists} *)

(** We need both these lists since path are right-assoc but
    query tuples are left-assoc.

    In each case in ('f,'r) list
    - 'f is the type of the whole function
    - 'r is the return type.

    'f is of the form [_ Converter.t ->  ... -> 'r]
*)

(** Prepend list of converters *)
type (_,_) convlist =
  | Nil : ('a,'a) convlist
  | Conv : ('a, 'b) Converter.t
      * (                        'l, 'r) convlist
     -> (('a, 'b) Converter.t -> 'l, 'r) convlist

(** Postpend list of converters *)
type (_,_) vonclist =
  | Nil : ('a, 'a) vonclist
  | Vonc : ('a, 'b) Converter.t
      * ('l, ('a, 'b) Converter.t -> 'r) vonclist
     -> ('l,                         'r) vonclist

(** We can write the statically typed rev_append.
    Notice how the type variables chain appropriately. *)
let rec rev_append
  : type c rc xc.
    (c, xc) vonclist -> (xc, rc) convlist -> (c, rc) convlist
  = fun l1 l2 -> match l1 with
    | Nil -> l2
    | Vonc (a,l) -> rev_append l (Conv(a,l2))

(** {2 The various types} *)

type (_,_) atom =
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

module Types = struct

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

  (** A convertible url is a path and a query (and potentially a slash).
      The type is the concatenation of both types.
  *)
  type ('f,'r,'c, 'rc) convurl =
    | Url : slash
        * ('f, 'x,     'c, 'xc     ) path_ty
        * (    'x, 'r,     'xc, 'rc) query_ty
       -> ('f,     'r, 'c,      'rc) convurl


end


(* We need the constructors in scope,
   disambiguation doesn't work on GADTs. *)
open! Types


(** A url is either
    - Convertible (without builtin converters)
    - Finalized (with builin converters)
*)
(* This type must be in this module (and not in Types)
   otherwise the typechecker can't prove exhaustiveness of some
   pattern matching.
*)
type ('f,'r, 'final, 'c,'rc) url_ty =
  | Convertible :
       ('f, 'r,         'c, 'rc) convurl
    -> ('f, 'r,[`NotF], 'c, 'rc) url_ty

  | Finalized :
       ('f, 'r, 'c, ('f, 'r, [`F], 'rc, 'rc) url_ty) convurl
     *         ('c, ('f, 'r, [`F], 'rc, 'rc) url_ty) convlist
    -> ('f, 'r, [`F], 'rc, 'rc) url_ty

(** {2 Combinators} *)

module Path = struct

  type ('f,'r,'c,'rc) t = ('f,'r,'c,'rc) Types.path_ty

  let host s = Host s
  let relative = Rel

  let add path b = PathConst(path,b)
  let add_atom path b = PathAtom(path,b)
  let add_conv path b = PathConv(path,b)

  let rec concat
    : type f r x c rc xc.
      (f,x,  c,xc   ) t ->
      (  x,r,  xc,rc) t ->
      (f,  r,c,   rc) t
    = fun p1 p2 -> match p2 with
      | Host _ -> p1
      | Rel  -> p1
      | PathConst (p,s) -> PathConst(concat p1 p, s)
      | PathAtom (p,a) -> PathAtom(concat p1 p, a)
      | PathConv (p,a) -> PathConv(concat p1 p, a)

end

module Query = struct

  type ('f,'r,'c,'rc) t = ('f,'r,'c,'rc) Types.query_ty

  let nil : _ t = Nil
  let any  = Any

  let add n x query = QueryAtom (n,x,query)
  let add_conv n x query = QueryConv (n,x,query)

  let rec make_any
    : type f r c rc. (f,r,c,rc) t -> (f,r,c,rc) t
    = function
      | Nil -> Any
      | Any -> Any
      | QueryAtom (n,x,q) -> QueryAtom(n,x,make_any q)
      | QueryConv (n,x,q) -> QueryConv(n,x,make_any q)

  let rec concat
    : type f r x c rc xc.
      (f,x,  c,xc   ) t ->
      (  x,r,  xc,rc) t ->
      (f,  r,c,   rc) t
    = fun q1 q2 -> match q1 with
      | Nil  -> q2
      | Any  -> make_any q2
      | QueryAtom (n,x,q) -> QueryAtom (n,x, concat q q2)
      | QueryConv (n,x,q) -> QueryConv (n,x, concat q q2)

end

module Url = struct

  type ('f,'r,'final,'c,'rc) t = ('f,'r,'final,'c,'rc) url_ty

  type slash = Types.slash = Slash | NoSlash | MaybeSlash

  let make ?(slash=NoSlash) path query : _ t =
    Convertible(Url (slash, path, query))

end

let nil = Query.nil
let any = Query.any
let ( ** )  (n,x) q = Query.add n x q
let ( **! ) (n,x) q = Query.add_conv n x q

let host = Path.host
let rel  = Path.relative
let (/)  = Path.add
let (/%) = Path.add_atom
let (/!) = Path.add_conv

let (/?) path query  = Url.make ~slash:NoSlash path query
let (//?) path query = Url.make ~slash:Slash path query
let (/??) path query = Url.make ~slash:MaybeSlash path query

let (~$) f = f ()

(** {2 Finalization} *)


(** An url with an empty list of converters.
    It can be evaluated/extracted/matched against.
 *)
type ('f, 'r, 'd, 'rc) t =
  ('f, 'r, 'd,'rc,'rc) Url.t
  constraint 'rc = _ Url.t



(** Finalization is the act of gathering all the converters in a list
    and bundling it with a convertible url.

    We use a typed prepend list for this with two typed variables
    which correspond to 'c and 'rc from earlier.

    We want to construct the list of converters
    corresponding to the [c] type variable.

    We will construct two lists:
    - A prepend list for the queries: {!convlist}
    - A postpend list for the path: {!vonclist}

    Our goal is to build the function that takes a convertible url,
    the list of convertible and returns a url.

    In practice: [('f, 'r, 'c, ('f, 'r) furl) url -> 'c].
    It justifies the return type of the conv list in {!url}.

    We proceed by CPS, passing the list of converters around
    while building a function to fill it.
*)
let rec finalize
  : type r f c. (f,r,_,c,(f, r, _, _) t) Url.t -> c
  = fun (Convertible(Url(slash, p, q))) ->
    finalize_path p @@ fun lp ->
    finalize_query q @@ fun lq ->
    Finalized (Url(slash,p,q), rev_append lp lq)

(** Once we have all these elements, it's simply type golf. *)

and finalize_path
  : type r f rc c.
    (f,r,c,rc) Path.t -> ((c,rc) vonclist -> rc) -> c
  = fun p k -> match p with
    | Host _ -> k Nil
    | Rel    -> k Nil
    | PathConst (p, _) -> finalize_path p k
    | PathAtom  (p, _) -> finalize_path p k
    | PathConv  (p, _) ->
      finalize_path p (fun l c -> k (Vonc(c,l)))

and finalize_query
  : type r f rc c.
    (f,r,c,rc) Query.t -> ((c,rc) convlist -> rc) -> c
  = fun p k -> match p with
    | Nil -> k Nil
    | Any -> k Nil
    | QueryAtom (_,_,q) -> finalize_query q k
    | QueryConv (_,_,q) ->
      (fun c -> finalize_query q (fun cl -> k (Conv (c,cl))))


(** {2 Evaluation functions} *)

(** Evaluation is the act of filling the holes.

    The process is rather straightforward using, once again, continuations.
*)

let rec eval_atom : type t a . (t,a) atom -> a -> string
  = function
  | Float  -> string_of_float
  | Int    -> string_of_int
  | Bool   -> string_of_bool
  | String -> (fun s -> s)
  (* TODO: We could pre-compile the regexp. *)
  | Regexp re ->
    fun s ->
      if not @@ Re.execp (Re.compile @@ Re.whole_string re) s
      then invalid_arg @@
        Printf.sprintf "Furl.eval: regexp not respected by \"%s\"." s ;
      s
  | Opt p -> (function None -> "" | Some x -> eval_atom p x)
  | Seq (p1,p2) ->
    (fun (x1,x2) -> eval_atom p1 x1 ^ eval_atom p2 x2)
  | Prefix(s,p) ->
    fun x -> s ^ eval_atom p x
  | Suffix(p,s) ->
    fun x -> eval_atom p x ^ s
  | Alt (pL, pR) ->
    (function `Left x -> eval_atom pL x | `Right x -> eval_atom pR x)
  | List p ->
    fun l -> String.concat "" @@ List.map (eval_atom p) l
  | List1 p ->
    fun (x,l) -> String.concat "" @@ List.map (eval_atom p) @@ x::l

let eval_top_atom : type a. ([`Top],a) atom -> a -> string list
  = function
  | Opt p -> (function None -> [] | Some x -> [eval_atom p x])
  | List p ->
    fun l -> List.map (eval_atom p) l
  | List1 p ->
    fun (x,l) -> List.map (eval_atom p) @@ x::l
  | e -> fun x -> [eval_atom e x]

let rec eval_path
  : type r f c xc.
    (f,r,c,xc) Path.t ->
    (c, 'rc) convlist ->
    ((xc, 'rc) convlist -> string option -> string list -> r) ->
    f
  = fun p l k -> match p with
    | Host s -> k l (Some s) []
    | Rel -> k l None []
    | PathConst (p, s) ->
      eval_path p l @@ fun l h r -> k l h (s :: r)
    | PathAtom (p, a) ->
      eval_path p l @@ fun l h r x ->
      k l h (eval_top_atom a x @ r)
    | PathConv(p, a) as _p ->
      eval_path p l @@ fun l h r x ->
      let Conv(c, l) = l in
      k l h (eval_top_atom a (c.of_ x) @ r)

let rec eval_query
  : type r f c xc.
    (f,r,c,xc) Query.t ->
    (c, 'rc) convlist ->
    ((xc, 'rc) convlist -> (string * string list) list -> r) ->
    f
  = fun q cl k -> match q with
    | Nil -> k cl []
    | Any -> k cl []
    | QueryAtom (n,a,q) ->
      fun x -> eval_query q cl @@ fun cl r ->
        k cl ((n, eval_top_atom a x) :: r)
    | QueryConv (n,a,q) ->
      let Conv(c, cl) = cl in
      fun x -> eval_query q cl @@ fun cl r ->
        k cl ((n, eval_top_atom a (c.of_ x)) :: r)

let keval_url
  : ('a, 'b, 'c, _ Url.t as 'url) convurl ->
    ('c, 'url) convlist -> (Uri.t -> 'b) -> 'a
  = fun (Url(slash,p,q)) cl k ->
    eval_path p cl @@ fun cl host path ->
    eval_query q cl @@ fun Nil query ->
    k @@
    let path = match slash with
      | Slash -> "" :: path
      | NoSlash
      | MaybeSlash -> path
    in Uri.make
      ?host
      ~path:(String.concat "/" @@ List.rev path)
      ~query ()

let keval
  : type final.  (_,_,final,_,_) Url.t -> _
  = fun furl k ->
    match furl with
      | Finalized (url, cl) -> keval_url url cl k
      | Convertible (url) ->  keval_url url Nil k

let eval url = keval url (fun x -> x)


(** {2 matching} *)

(** Matching is the act of extracting the information contained in a url
    using a formatted url.

    This is not straightforward.

    We proceed in two steps:
    1. Construct a regular expression matching the desired url.
    2. Extract the information from the substrings once the url is matched.
*)

(** {3 Regexp construction}

    The functions associated with this step are named re_*
    In order to record how we constructed the regexp and how to later
    extract information, we build a witness containing all the tools we need.

    For each types (atom, query, path, uri), these witnesses are named re_*.

    {4 Principles of construction of the regexp}

    Each alternative is marked with {!Re.mark}. We store the markid in order
    to be able to guess the branch matched.
    The path is simply a concatenation of the regular expressions, separated
    by /, with the particular treatment of lists.

    query elements can appear in any order, so we reorder the
    key by alphabetical order (both in the incoming query and the extraction).
    We register the permutation as a mapping from indexes to matching group.
*)

(** The sorting criteria for queries. It must be used both for
    regexp construction and extraction.
*)
let sort_query l =
  List.sort (fun (x,_) (y,_) -> compare (x: string) y) l

type _ re_atom =
  | Float     : float re_atom
  | Int       : int re_atom
  | Bool      : bool re_atom
  | String    : string re_atom
  | Regexp    : Re.t -> string re_atom
  | Opt       : Re.markid * 'a re_atom -> 'a option re_atom
  | Alt       :
      Re.markid * 'a re_atom * Re.markid * 'b re_atom
    -> [`Left of 'a| `Right of 'b] re_atom
  | Seq       :
      'a re_atom * 'b re_atom -> ('a * 'b) re_atom
  | Nest      : 'a re_atom -> 'a re_atom

  | List      : 'a re_atom * Re.re -> 'a list re_atom
  | List1     : 'a re_atom * Re.re -> ('a * 'a list) re_atom

(** Count the matching groups the regexp encoding some atom will have. *)
let rec count_group
  : type a. a re_atom -> int
  = function
    | Float  -> 1
    | Int  -> 1
    | Bool  -> 1
    | String  -> 1
    | Regexp _ -> 1
    | Opt (_,e) -> count_group e
    | Alt (_,e1,_,e2) -> count_group e1 + count_group e2
    | Seq (e1,e2) -> count_group e1 + count_group e2
    | Nest e -> count_group e
    | List _ -> 1
    | List1 _ -> 1

let incrg e i = i + count_group e


let rec re_atom
  : type t a. component:_ -> (t,a) atom -> a re_atom * Re.t
  = let open Re in fun ~component -> function
    | Float       -> Float, group Furl_re.float
    | Int         -> Int, group Furl_re.arbitrary_int
    | Bool        -> Bool, group Furl_re.bool
    | String      -> String, group @@ Furl_re.string component
    | Regexp re   -> Regexp re, group @@ no_group re
    | Opt e       ->
      let w, (id, re) = map_snd mark @@ re_atom ~component e in
      Opt (id,w), alt [epsilon ; re]
    | Alt (e1,e2)  ->
      let w1, (id1, re1) = map_snd mark @@ re_atom ~component e1 in
      let w2, (id2, re2) = map_snd mark @@ re_atom ~component e2 in
      Alt (id1, w1, id2, w2), alt [re1 ; re2]
    | Prefix (s,e)->
      let w, re = re_atom ~component e in
      Nest w, seq [str s ; re]
    | Suffix (e,s)->
      let w, re = re_atom ~component e in
      Nest w, seq [re ; str s]
    | Seq (e1,e2) ->
      let w1, re1 = re_atom ~component e1 in
      let w2, re2 = re_atom ~component e2 in
      Seq (w1, w2), seq [re1; re2]

    (* top *)
    | List e      ->
      let w, re = re_atom ~component e in
      List (w,Re.compile re), group @@ rep @@ no_group re
    | List1 e     ->
      let w, re = re_atom ~component e in
      List1 (w,Re.compile re), group @@ rep1 @@ no_group re

(** Top level atoms are specialized for path and query, see documentation. *)

let re_atom_path
  : type a . (_,a) atom -> a re_atom * Re.t
  =
  let open Re in
  let component = `Path in
  function
    | List  e ->
      let w, re = re_atom ~component e in
      List (w, Re.compile re),
      group @@ Furl_re.list ~component 0 @@ no_group re
    | List1 e ->
      let w, re = re_atom ~component e in
      List1 (w, Re.compile re),
      group @@ Furl_re.list ~component 1 @@ no_group re
    | Opt e ->
      let w, re = re_atom ~component e in
      let id, re = mark re in
      Opt (id,w), seq [alt [epsilon ; seq [Furl_re.slash ; re]]]
    | e ->
      let w, re = re_atom ~component e in
      w, seq [Furl_re.slash; re]

let re_atom_query
  : type a . (_,a) atom -> a re_atom * Re.t
  =
  let open Re in
  let component = `Query_value in
  function
    | List  e ->
      let w, re = re_atom ~component e in
      List (w, Re.compile re),
      group @@ Furl_re.list ~component 0 @@ no_group re
    | List1 e ->
      let w, re = re_atom ~component e in
      List1 (w, Re.compile re),
      group @@ Furl_re.list ~component 1 @@ no_group re
    | e ->
      let w, re = re_atom ~component e in
      w, re


type (_,_,_,_) re_path =
  | Start : ('r,'r, 'rc, 'rc) re_path
  | PathAtom :
       ('f, 'a -> 'r, 'c, 'rc) re_path * int * 'a re_atom
    -> ('f,       'r, 'c, 'rc) re_path
  | PathConv :
       ('f, 'b -> 'r, 'c, ('a, 'b) Converter.t -> 'rc) re_path
      * int * 'a re_atom
    -> ('f,       'r, 'c,                         'rc) re_path

let rec re_path
  : type r f rc fc .
    (f, r, fc, rc) Path.t ->
    (f, r, fc, rc) re_path * int * Re.t list
  = let open Re in function
    | Host s ->
      let re = Re.str @@ Uri.pct_encode ~component:`Host s in
      Start, 1, [re]
    | Rel    -> Start, 1, []
    | PathConst (p,s) ->
      let (p, nb_group, re) = re_path p in
      p, nb_group, str s :: Furl_re.slash :: re
    | PathAtom (p,a) ->
      let wa, ra = re_atom_path a in
      let (wp, nb_group, rp) = re_path p in
      PathAtom (wp, nb_group, wa), incrg wa nb_group, ra :: rp
    | PathConv (p,a) ->
      let wa, ra = re_atom_path a in
      let (wp, nb_group, rp) = re_path p in
      PathConv (wp, nb_group, wa), incrg wa nb_group, ra :: rp


type ('fu,'ret,'converter,'retc) re_query =
  | Nil  : ('r,'r,'rc, 'rc) re_query
  | Any  : ('r,'r,'rc, 'rc) re_query
  | Cons :
      'a re_atom * ('f,'r,'c,'rc) re_query
    -> ('a -> 'f,'r,'c,'rc) re_query
  | Conv : 'a re_atom * ('f,'r,'c,'rc) re_query
    -> ('b -> 'f, 'r, ('a, 'b) Converter.t -> 'c, 'rc) re_query

let rec re_query
  : type r f rc fc .
    (f, r, fc, rc) Query.t ->
    (f, r, fc, rc) re_query * bool * (string * (Re.t * int)) list
  = function
    | Nil -> Nil, false, []
    | Any -> Any, true,  []
    | QueryAtom (s,a,q) ->
      let wa, ra = re_atom_query a in
      let wq, b_any, rq = re_query q in
      Cons (wa, wq), b_any, (s, (ra, count_group wa)) :: rq
    | QueryConv (s,a,q) ->
      let wa, ra = re_atom_query a in
      let wq, b_any, rq = re_query q in
      Conv (wa, wq), b_any, (s,(ra, count_group wa)) :: rq


type ('f,'r,'c,'rc) re_url =
  | ReUrl :
       ('f, 'x,    'c, 'xc     ) re_path
     * (    'x, 'r,    'xc, 'rc) re_query * int array
    -> ('f,     'r,'c,      'rc) re_url

let re_url
  : type f r c rc. (f,r,c,rc) convurl -> (f,r,c,rc) re_url * Re.t
  = function Url(slash,p,q) ->
    let end_path = match slash with
      | NoSlash -> Re.epsilon
      | Slash -> Re.char '/'
      | MaybeSlash -> Re.(opt @@ char '/')
    in
    let (wp, nb_group, rp) = re_path p in
    match q with
      | Nil ->
        ReUrl (wp, Nil, [||]),
        Re.seq @@ List.rev (end_path :: rp)

      | Any ->
        let end_re = Re.(opt @@ seq [Re.char '?' ; rep any]) in
        ReUrl (wp, Nil, [||]),
        Re.seq @@ List.rev_append rp [end_path; end_re]

      | _ ->
        let (wq, any_query, rql) = re_query q in
        let rel = sort_query rql in
        let t =
          build_permutation nb_group (fun (_,(_,i)) -> i) rql rel
        in

        let query_sep = Furl_re.query_sep any_query in
        let add_around_query =
          if not any_query then fun x -> x
          else fun l -> Re.(rep any) :: l
        in

        let re =
          rel
          |> List.fold_left (fun l (s,(re,_)) ->
            Re.seq [Re.str (s ^ "=") ; re ] :: l
          ) []
          |> intersperse query_sep
          |> add_around_query
          |> List.rev
          |> add_around_query
        in
        ReUrl(wp,wq,t),
        Re.seq @@ List.rev_append rp (end_path :: Re.char '?' :: re)

let get_re
  : type r f final. (f, r, final, _) t -> Re.t
  = function
    | Convertible url -> snd @@ re_url url
    | Finalized (url,_) -> snd @@ re_url url


(** {3 Extraction.} *)

(** Extracting atom is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.
*)
let rec extract_atom
  : type a. a re_atom -> int -> Re.substrings -> int * a
  = fun rea i s -> match rea with
    | Float  -> incrg rea i, float_of_string (Re.get s i)
    | Int    -> incrg rea i, int_of_string   (Re.get s i)
    | Bool   -> incrg rea i, bool_of_string  (Re.get s i)
    | String -> incrg rea i, Re.get s i
    | Regexp _ -> incrg rea i, Re.get s i
    | Opt (id,w) ->
      if not @@ Re.marked s id then incrg rea i, None
      else map_snd (fun x -> Some x) @@ extract_atom w i s
    | Alt (i1,w1,id2,w2) ->
      if Re.marked s i1 then
        map_snd (fun x -> `Left x) @@ extract_atom w1 i s
      else if Re.marked s id2 then
        map_snd (fun x -> `Right x) @@ extract_atom w2 (incrg w1 i) s
      else
        (* Invariant: Alt produces [Re.alt [e1 ; e2]] *)
        assert false
    | Seq (e1,e2) ->
      let i, v1 = extract_atom e1 i s in
      let i, v2 = extract_atom e2 i s in
      i, (v1, v2)
    | Nest e -> extract_atom e i s
    | List (e,re) -> i+1, extract_list e re i s
    | List1 (e,re) ->
      match extract_list e re i s with
        | h :: t -> incrg rea i, (h, t)
        | [] ->
          (* Invariant: List1 produces [Re.rep1 e] *)
          assert false

(** We need to re-match the string for lists, in order to extract
    all the elements.
    Re doesn't offer the possibility to keep the results when
    grouping under a star (one could argue it's theoretically not
    possible as it would be equivalent to counting in an automaton).
*)
and extract_list
  : type a. a re_atom -> Re.re -> int -> Re.substrings -> a list
  = fun e re i s ->
    let aux s = snd @@ extract_atom e 1 s in
    let (pos, pos') = Re.get_ofs s i in
    let len = pos' - pos in
    (* The whole original string*)
    let original = Re.get s 0 in
    Gen.to_list @@ Gen.map aux @@ Re.all_gen ~pos ~len re original


(** Since path is in reversed order, we proceed by continuation.
*)
let rec extract_path
  : type f x r c xc xc'.
    (f,x,c,xc) re_path ->
    Re.substrings ->
    ((xc, _) convlist -> x -> r * (xc', _) convlist) ->
      (c, _) convlist -> f -> r * (xc', _) convlist
  = fun wp subs k -> match wp with
    | Start  -> k
    | PathAtom (rep, idx, rea) ->
      let _, v = extract_atom rea idx subs in
      let k cl f = k cl (f v) in
      extract_path rep subs k
    | PathConv (rep, idx, rea) ->
      let _, v = extract_atom rea idx subs in
      let k (Conv (conv,cl) : _ convlist) f =
        k cl (f @@ conv.to_ v)
      in
      extract_path rep subs k

(** Query are in the right order, we can proceed in direct style. *)
let rec extract_query
  : type x r xc rc.
    (x,r,xc,rc) re_query ->
    int -> Re.substrings -> int array ->
    (xc, _) convlist ->
    x -> r * (rc, _) convlist
  = fun wq i subs permutation cl f -> match wq with
    | Nil  -> f, cl
    | Any  -> f, cl
    | Cons (rea,req) ->
      let subs_idx = permutation.(i) in
      let _, v = extract_atom rea subs_idx subs in
      extract_query req (i+1) subs permutation cl (f v)

    | Conv (rea,req) ->
      let subs_idx = permutation.(i) in
      let _, v = extract_atom rea subs_idx subs in
      let Conv (conv,cl) = cl in
      extract_query req (i+1) subs permutation cl (f @@ conv.to_ v)

let extract_url
  : type r f fc.
    (f, r, fc, _ Url.t as 'rc) re_url ->
    (fc, 'rc) convlist ->
    Re.substrings -> f -> r
  = fun (ReUrl (wp, wq, permutation)) cl subs f ->
    let k = extract_query wq 0 subs permutation in
    let r, Nil = extract_path wp subs k cl f in
    r

let prepare_uri uri =
  uri
  |> Uri.query
  |> sort_query
  |> Uri.with_query uri
  |> Uri.path_and_query

let extract_convertible
  : type r f fc. (f, r, fc, _) convurl ->
    (fc, _) convlist ->
    f:f -> Uri.t -> r
  = fun url cl ->
    let re_url, re = re_url url in
    let re = Re.(compile @@ whole_string re) in
    fun ~f uri ->
      let s = prepare_uri uri in
      let subs = Re.exec re s in
      extract_url re_url cl subs f

let extract
  : type r f final. (f, r, final, _) t -> f:f -> Uri.t -> r
  = function
    | Convertible url -> extract_convertible url Nil
    | Finalized (url, cl) -> extract_convertible url cl

(** {4 Multiple match} *)

type 'r route = Route : ('f, 'r, _, _) t * 'f -> 'r route

let route url f = Route (url, f)

let (-->) = route

type 'r re_ex =
    ReEx :
      'f * Re.markid * ('f, 'r, 'c, _ Url.t as 'rc) re_url
      * ('c, 'rc) convlist -> 'r re_ex


(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
let rec build_info_list = function
  | [] -> [], []
  | Route (url, f) :: l ->
    let rel, wl = build_info_list l in
    match url with
      | Convertible url -> begin
          let re_url, re = re_url url in
          let id, re = Re.mark re in
          re::rel, ReEx (f, id, re_url, Nil)::wl
        end
      | Finalized (url, cl) -> begin
          let re_url, re = re_url url in
          let id, re = Re.mark re in
          re::rel, ReEx (f, id, re_url, cl)::wl
        end

let rec find_and_trigger
  : type r. Re.substrings -> r re_ex list -> r
  = fun subs -> function
    | [] ->
      (* Invariant: At least one of the regexp of the alternative matches. *)
      assert false
    | ReEx (f, id, re_url, cl) :: l ->
      if Re.marked subs id then extract_url re_url cl subs f
      else find_and_trigger subs l

let match_url
  : type r.
    default:(Uri.t -> r) -> r route list -> Uri.t -> r
  = fun ~default l ->
    let rel, wl = build_info_list l in
    let re = Re.(compile @@ whole_string @@ alt rel) in
    fun uri ->
      let s = prepare_uri uri in
      try
        let subs = Re.exec re s in
        find_and_trigger subs wl
      with
          Not_found -> default uri
