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

(** {2 The various types} *)

type 'a atom = 'a Tyre.t

module Types = struct

  (* type ('f, 'r) atom = *)
  (*   | PathConst : string -> ('r, 'r) atom *)
  (*   | Path : 'a Tyre.t -> ('r, 'r -> 'a) atom *)

  type ('fu, 'return) path =
    | Host : string -> ('r, 'r) path
    | Rel  : ('r, 'r) path
    | PathConst :
        ('f, 'r) path * string
     -> ('f, 'r) path
    | PathAtom :
        ('f,'a -> 'r) path * 'a atom
     -> ('f,      'r) path

  type ('fu, 'return) query =
    | Nil  : ('r,'r) query
    | Any  : ('r,'r) query
    | QueryAtom : string * 'a atom
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


(* We need the constructors in scope,
   disambiguation doesn't work on GADTs. *)
open Tyre.Internal
open Types

(** {2 Combinators} *)

module Path = struct

  type ('f,'r) t = ('f,'r) Types.path

  let host s = Host s
  let relative = Rel

  let add path b = PathConst(path,b)
  let add_atom path b = PathAtom(path,b)

  let rec concat
    : type f r x.
      (f,x  ) t ->
      (  x,r) t ->
      (f,  r) t
    = fun p1 p2 -> match p2 with
      | Host _ -> p1
      | Rel  -> p1
      | PathConst (p,s) -> PathConst(concat p1 p, s)
      | PathAtom (p,a) -> PathAtom(concat p1 p, a)

end

module Query = struct

  type ('f,'r) t = ('f,'r) Types.query

  let nil : _ t = Nil
  let any  = Any

  let add n x query = QueryAtom (n,x,query)

  let rec make_any
    : type f r . (f,r) t -> (f,r) t
    = function
      | Nil -> Any
      | Any -> Any
      | QueryAtom (n,x,q) -> QueryAtom(n,x,make_any q)

  let rec concat
    : type f r x.
      (f,x  ) t ->
      (  x,r) t ->
      (f,  r) t
    = fun q1 q2 -> match q1 with
      | Nil  -> q2
      | Any  -> make_any q2
      | QueryAtom (n,x,q) -> QueryAtom (n,x, concat q q2)

end

module Url = struct

  type ('f,'r) t = ('f,'r) url

  type slash = Types.slash = Slash | NoSlash | MaybeSlash

  let make ?(slash=NoSlash) path query : _ t =
    Url (slash, path, query)

  let prefix_path path = function
    | Url (slash, path', query) ->
      Url (slash, Path.concat path path', query)

  let add_query query = function
    | Url (slash, path, query') ->
      Url (slash, path, Query.concat query' query)

end

let nil = Query.nil
let any = Query.any
let ( ** )  (n,x) q = Query.add n x q

let host = Path.host
let rel  = Path.relative
let (/)  = Path.add
let (/%) = Path.add_atom

let (/?) path query  = Url.make ~slash:NoSlash path query
let (//?) path query = Url.make ~slash:Slash path query
let (/??) path query = Url.make ~slash:MaybeSlash path query

let (~$) f = f ()

(** {2 Finalization} *)


(** An url with an empty list of converters.
    It can be evaluated/extracted/matched against.
 *)
type ('f, 'r) t = ('f, 'r) Url.t


(** {2 Evaluation functions} *)

(** Evaluation is the act of filling the holes.

    The process is rather straightforward using, once again, continuations.
*)

let eval_atom p x = Tyre.(eval (Internal.to_t p) x)

let eval_top_atom : type a. a raw -> a -> string list
  = function
  | Opt p -> (function None -> [] | Some x -> [eval_atom p x])
  | Rep p ->
    fun l -> Gen.to_list @@ Gen.map (eval_atom p) l
  | e -> fun x -> [eval_atom e x]

let rec eval_path
  : type r f.
    (f,r) Path.t ->
    (string option -> string list -> r) ->
    f
  = fun p k -> match p with
    | Host s -> k (Some s) []
    | Rel -> k None []
    | PathConst (p, s) ->
      eval_path p @@ fun h r -> k h (s :: r)
    | PathAtom (p, a) ->
      eval_path p @@ fun h r x ->
      k h (eval_top_atom (from_t a) x @ r)

let rec eval_query
  : type r f.
    (f,r) Query.t ->
    ((string * string list) list -> r) ->
    f
  = fun q k -> match q with
    | Nil -> k []
    | Any -> k []
    | QueryAtom (n,a,q) ->
      fun x -> eval_query q @@ fun r ->
        k ((n, eval_top_atom (from_t a) x) :: r)

let keval
  : ('a, 'b) url -> (Uri.t -> 'b) -> 'a
  = fun (Url(slash,p,q)) k ->
    eval_path p @@ fun host path ->
    eval_query q @@ fun query ->
    k @@
    let path = match slash with
      | Slash -> "" :: path
      | NoSlash
      | MaybeSlash -> path
    in Uri.make
      ?host
      ~path:(String.concat "/" @@ List.rev path)
      ~query ()

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

type 'a re_atom = 'a Tyre.Internal.wit

let re_atom re = Tyre.Internal.build re
(** Top level atoms are specialized for path and query, see documentation. *)

let re_atom_path
  : type a . a raw -> int * a re_atom * Re.t
  =
  let open Re in
  function
    | Rep e ->
      let grps, w, re = re_atom e in
      grps, Rep (w, Re.compile re),
      group @@ Furl_re.list ~component:`Path 0 @@ no_group re
    | Opt e ->
      let grps, w, re = re_atom e in
      let id, re = mark re in
      grps, Opt (id,grps,w),
      seq [alt [epsilon ; seq [Furl_re.slash ; re]]]
    | e ->
      let grps, w, re = re_atom e in
      grps, w, seq [Furl_re.slash; re]

let re_atom_query
  : type a . a raw -> int * a re_atom * Re.t
  =
  let open Re in
  function
    | Rep e ->
      let grps, w, re = re_atom e in
      grps, Rep (w, Re.compile re),
      group @@ Furl_re.list ~component:`Query_value 0 @@ no_group re
    | e -> re_atom e


type (_,_) re_path =
  | Start : ('r,'r) re_path
  | PathAtom :
       ('f, 'a -> 'r) re_path * int * 'a re_atom
    -> ('f,       'r) re_path

let rec re_path
  : type r f .
    (f, r) Path.t ->
    int * (f, r) re_path * Re.t list
  = let open Re in function
    | Host s ->
      let re = Re.str @@ Uri.pct_encode ~component:`Host s in
      1, Start, [re]
    | Rel    -> 1, Start, []
    | PathConst (p,s) ->
      let (grps, p, re) = re_path p in
      grps, p,
      str s :: Furl_re.slash :: re
    | PathAtom (p,a) ->
      let grps, wa, ra = re_atom_path @@ from_t a in
      let (path_grps, wp, rp) = re_path p in
      grps + path_grps,
      PathAtom (wp, path_grps, wa),
      ra :: rp


type ('fu,'ret) re_query =
  | Nil  : ('r,'r) re_query
  | Any  : ('r,'r) re_query
  | Cons :
      'a re_atom * ('f,'r) re_query
    -> ('a -> 'f,'r) re_query

let rec re_query
  : type r f .
    (f, r) Query.t ->
    (f, r) re_query * bool * (string * (Re.t * int)) list
  = function
    | Nil -> Nil, false, []
    | Any -> Any, true,  []
    | QueryAtom (s,a,q) ->
      let grps, wa, ra = re_atom_query @@ from_t a in
      let wq, b_any, rq = re_query q in
      Cons (wa, wq), b_any, (s, (ra, grps)) :: rq


type ('f,'r) re_url =
  | ReUrl :
       ('f, 'x    ) re_path
     * (    'x, 'r) re_query * int array
    -> ('f,     'r) re_url

let re_url
  : type f r. (f,r) Url.t -> (f,r) re_url * Re.t
  = function Url(slash,p,q) ->
    let end_path = match slash with
      | NoSlash -> Re.epsilon
      | Slash -> Re.char '/'
      | MaybeSlash -> Re.(opt @@ char '/')
    in
    let path_grps, wp, rp = re_path p in
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
          build_permutation path_grps (fun (_,(_,i)) -> i) rql rel
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

let get_re url = snd @@ re_url url

(** {3 Extraction.} *)

(** Extracting atom is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.
*)
let extract_atom = extract

(** Since path is in reversed order, we proceed by continuation.
*)
let rec extract_path
  : type f x r.
    (f,x) re_path ->
    Re.substrings ->
    (x -> r) ->
      (f -> r)
  = fun wp subs k -> match wp with
    | Start  -> k
    | PathAtom (rep, idx, rea) ->
      let _, v = extract_atom rea idx subs in
      let k f = k (f v) in
      extract_path rep subs k

(** Query are in the right order, we can proceed in direct style. *)
let rec extract_query
  : type x r.
    (x,r) re_query ->
    int -> Re.substrings -> int array ->
    x -> r
  = fun wq i subs permutation f -> match wq with
    | Nil  -> f
    | Any  -> f
    | Cons (rea,req) ->
      let subs_idx = permutation.(i) in
      let _, v = extract_atom rea subs_idx subs in
      extract_query req (i+1) subs permutation (f v)


let extract_url
  : type r f.
    (f, r) re_url ->
    Re.substrings -> f -> r
  = fun (ReUrl (wp, wq, permutation)) subs f ->
    let k = extract_query wq 0 subs permutation in
    let r = extract_path wp subs k f in
    r

let prepare_uri uri =
  uri
  |> Uri.query
  |> sort_query
  |> Uri.with_query uri
  |> Uri.path_and_query

let extract url =
  let re_url, re = re_url url in
  let re = Re.(compile @@ whole_string re) in
  fun ~f uri ->
    let s = prepare_uri uri in
    let subs = Re.exec re s in
    extract_url re_url subs f

(** {4 Multiple match} *)

type 'r route = Route : ('f, 'r) t * 'f -> 'r route

let route url f = Route (url, f)

let (-->) = route

type 'r re_ex =
    ReEx : 'f * Re.markid * ('f, 'r) re_url -> 'r re_ex


(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
let rec build_info_list = function
  | [] -> [], []
  | Route (url, f) :: l ->
    let rel, wl = build_info_list l in
    let re_url, re = re_url url in
    let id, re = Re.mark re in
    re::rel, ReEx (f, id, re_url)::wl

let rec find_and_trigger
  : type r. Re.substrings -> r re_ex list -> r
  = fun subs -> function
    | [] ->
      (* Invariant: At least one of the regexp of the alternative matches. *)
      assert false
    | ReEx (f, id, re_url) :: l ->
      if Re.marked subs id then extract_url re_url subs f
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
