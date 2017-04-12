open Furl_utils
module SMap = Map.Make(String)

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
   - w is for witness (see section matching)
   - k is for kontinuation (with a k).

   Merlin is heavily recommended to browse this code.
*)

(** {2 The various types} *)

type 'a atom = 'a Tyre.t

module Types = struct

  type ('fu, 'return) url =
    | Host : string -> ('r, 'r) url
    | Rel  : ('r, 'r) url
    | PathConst :
        ('f, 'r) url * string
     -> ('f, 'r) url
    | PathAtom :
        ('f,'a -> 'r) url * 'a atom
     -> ('f,      'r) url
    | QueryAtom :
        ('f, 'a -> 'r) url * string * 'a atom
     -> ('f,       'r) url

end


(* We need the constructors in scope,
   disambiguation doesn't work on GADTs. *)
module TI = Tyre.Internal
open Types

type ('f,'r) t = ('f,'r) url

let host s = Host s
let rel = Rel

let add u b = PathConst(u,b)
let add_atom u b = PathAtom(u,b)
let add_query u (s,b) = QueryAtom(u,s,b)

let (/)  = add
let (/%) = add_atom
let (/?) = add_query

let rec concat
  : type f r x.
    (f,x  ) t ->
    (  x,r) t ->
    (f,  r) t
  = fun u1 u2 -> match u2 with
    | Host _ -> u1
    | Rel  -> u1
    | PathConst (u,s) -> PathConst(concat u1 u, s)
    | PathAtom (u,a) -> PathAtom(concat u1 u, a)
    | QueryAtom (u, s, a) -> QueryAtom(concat u1 u, s, a)

let (~$) f = f ()



(** {2 Evaluation functions} *)

(** Evaluation is the act of filling the holes.

    The process is rather straightforward using, once again, continuations.
*)

let eval_atom p x = Tyre.(eval (Internal.to_t p) x)

let rec eval_top_atom : type a. a TI.raw -> a -> string list
  = function
  | TI.Opt p -> (function None -> [] | Some x -> [eval_atom p x])
  | TI.Rep p ->
    fun l -> Gen.to_list @@ Gen.map (eval_atom p) l
  | TI.Conv (_s, p, conv) ->
    fun x -> eval_top_atom p (conv.from_ x)
  | TI.Mod (_,p) -> eval_top_atom p
  | e -> fun x -> [eval_atom e x]

let rec eval_raw
  : type r f.
    (f,r) t ->
    (string option -> string list -> (string * string list) list -> r) ->
    f
  = fun p k -> match p with
    | Host s -> k (Some s) [] []
    | Rel -> k None [] []
    | PathConst (p, s) ->
      eval_raw p @@ fun h p' q' -> k h (s :: p') q'
    | PathAtom (p, a) ->
      eval_raw p @@ fun h p' q' x ->
      k h (eval_top_atom (TI.from_t a) x @ p') q'
    | QueryAtom (p, s, a) ->
      eval_raw p @@ fun h p' q' x ->
      k h p' ((s, eval_top_atom (TI.from_t a) x) :: q')

let keval
  : ('a, 'b) url -> (Uri.t -> 'b) -> 'a
  = fun u k ->
    let k host path query =
      k @@ Uri.make
        ?host
        ~path:(String.concat "/" @@ List.rev path)
        ~query ()
    in
    eval_raw u k

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
  : type a . a TI.raw -> int * a re_atom * Re.t
  =
  let open Re in
  function
    | TI.Rep e ->
      let grps, w, re = re_atom e in
      grps, Rep (w, Re.compile re),
      group @@ Furl_re.list ~component:`Path 0 @@ no_group re
    | TI.Opt e ->
      let grps, w, re = re_atom e in
      let id, re = mark re in
      grps, Opt (id,grps,w),
      seq [alt [epsilon ; seq [Furl_re.slash ; re]]]
    | e ->
      let grps, w, re = re_atom e in
      grps, w, seq [Furl_re.slash; re]

let re_atom_query
  : type a . a TI.raw -> int * a re_atom * Re.t
  =
  let open Re in
  function
    | TI.Rep e ->
      let grps, w, re = re_atom e in
      grps, Rep (w, Re.compile re),
      group @@ Furl_re.list ~component:`Query_value 0 @@ no_group re
    | e -> re_atom e


type (_,_) wit =
  | Start : ('r,'r) wit
  | Atom :
       ('f, 'a -> 'r) wit * int * 'a re_atom
    -> ('f,       'r) wit
  | Query :
       ('f, 'a -> 'r) wit * string * 'a re_atom
    -> ('f,       'r) wit

type ('f,'r) info = {
  wit : ('f, 'r) wit ;
  query_indices : int SMap.t ;
  re : Re.t ;
  next_idx : int ;
}

(* Return:
   - The next group index for paths
   - The witness
   - A list of regular expression for each path segments in reverse order
   - A map of query parameters to regular expressions
*)
let rec make_witness
  : type r f .
    int -> (f, r) t ->
    int * (f, r) wit * Re.t list * (Re.t * int) SMap.t
  = let open Re in fun idx_start -> function
    | Host s ->
      let re = Re.str @@ Uri.pct_encode ~component:`Host s in
      idx_start, Start, [re], SMap.empty
    | Rel    -> idx_start, Start, [], SMap.empty
    | PathConst (p,s) ->
      let grps, p, re, qmap = make_witness idx_start p in
      grps, p,
      str s :: Furl_re.slash :: re,
      qmap
    | PathAtom (p,a) ->
      let grps, wa, ra = re_atom_path @@ TI.from_t a in
      let path_grps, wp, rp, qmap = make_witness idx_start p in
      grps + path_grps,
      Atom (wp, path_grps, wa),
      ra :: rp,
      qmap
    | QueryAtom (p, s, a) ->
      let grps_a, wa, ra = re_atom_query @@ TI.from_t a in
      let grps, wp, rp, qmap = make_witness idx_start p in
      grps,
      Query (wp, s, wa),
      rp,
      (SMap.add s (ra, grps_a) qmap)

let re_url
  : type f r. int -> (f,r) t -> (f,r) info
  = fun idx_start url ->
    let next_path_idx, wit, rp, qmap = make_witness idx_start url in
    let end_path = Re.char '/' in
    if SMap.is_empty qmap then
      let next_idx = next_path_idx in
      let re =
        let params = Re.(opt @@ seq [Re.char '?' ; rep any]) in
        let end_re = Re.(opt @@ seq [end_path; params]) in
        Re.seq @@ List.rev_append rp [end_re]
      in
      { wit ; re ; query_indices = SMap.empty ; next_idx }
    else
      let rql = SMap.bindings qmap in
      let rel = sort_query rql in
      let query_indices, next_idx =
        List.fold_left
          (fun (m, offset) (name, (_, size)) ->
             (SMap.add name offset m), offset+size
          )
          (SMap.empty, next_path_idx)
          rel
      in

      let add_around_query l = Re.(rep any) :: l in
      let re =
        rel
        |> List.fold_left (fun l (s,(re,_)) ->
          Re.seq [Re.str (s ^ "=") ; re ] :: l
        ) []
        |> intersperse Furl_re.query_sep
        |> add_around_query
        |> List.rev
        |> add_around_query
      in
      let re =
        Re.seq @@ List.rev_append rp (end_path :: Re.char '?' :: re)
      in
      { wit ; re ; query_indices ; next_idx }

(* let rec shift_groups_wit *)
(*   : type f r. int -> (f, r) wit -> (f, r) wit *)
(*   = fun i x -> match x with *)
(*     | Start -> Start *)
(*     | Atom (w,idx,re) -> *)
(*       Atom (shift_groups_wit i w, i+idx, shift_groups_atoms i re) *)
(*     | Query (w,s,re) -> *)
(*       Query (shift_groups_wit i w, s, shift_groups_atoms i re) *)

(* and shift_groups_atoms *)
(*   : type r. int -> r re_atom -> r re_atom *)
(*   = fun i x -> match x with *)
(*     | TI.Regexp x -> (??) *)
(*     | TI.Conv (_,_,_) -> (??) *)
(*     | TI.Opt (_,_,_) -> (??) *)
(*     | TI.Alt (_,_,_,_,_) -> (??) *)
(*     | TI.Seq (_,_) -> (??) *)
(*     | TI.Rep (_,_) -> (??) *)

(** {3 Extraction.} *)

(** Extracting atom is just a matter of following the witness.
    We just need to take care of counting where we are in the matching groups.
*)
let extract_atom = Tyre.Internal.extract

(** Since path is in reversed order, we proceed by continuation.
*)
let rec extract_url
  : type f x r f' x'.
    original:string ->
  (f',x') info ->
  (f,x) wit -> Re.substrings ->
  (x -> r) -> (f -> r)
  = fun ~original info w subs k -> match w with
    | Start  -> k
    | Atom (wit, idx, rea) ->
      let _, v = extract_atom ~original rea idx subs in
      let k f = k (f v) in
      extract_url ~original info wit subs k
    | Query (wit, name, rea) ->
      let subs_idx = SMap.find name info.query_indices in
      let _, v = extract_atom ~original rea subs_idx subs in
      let k f = k (f v) in
      extract_url ~original info wit subs k

let extract_url ~original info subs =
  extract_url ~original info info.wit subs (fun x -> x)

let prepare_uri uri =
  uri
  |> Uri.query
  |> sort_query
  |> Uri.with_query uri
  |> Uri.path_and_query

let extract url =
  let info = re_url 1 url in
  let re = Re.(compile @@ whole_string info.re) in
  fun ~f uri ->
    let s = prepare_uri uri in
    let subs = Re.exec re s in
    extract_url ~original:s info subs f

(** {4 Multiple match} *)

type 'r route = Route : ('f, 'r) t * 'f -> 'r route

let route url f = Route (url, f)

let (-->) = route

type 'r re_ex =
    ReEx : 'f * Re.markid * ('f, 'r) info -> 'r re_ex


(* It's important to keep the order here, since Re will choose
   the first regexp if there is ambiguity.
*)
let rec build_info_list idx = function
  | [] -> [], []
  | Route (url, f) :: l ->
    let info = re_url idx url in
    let rel, wl = build_info_list info.next_idx l in
    let id, re = Re.mark info.re in
    re::rel, ReEx (f, id, info)::wl

let rec find_and_trigger
  : type r. original:string -> Re.substrings -> r re_ex list -> r
  = fun ~original subs -> function
    | [] ->
      (* Invariant: At least one of the regexp of the alternative matches. *)
      assert false
    | ReEx (f, id, info) :: l ->
      if Re.marked subs id
      then extract_url ~original info subs f
      else find_and_trigger ~original subs l

let match_url
  : type r.
    default:(Uri.t -> r) -> r route list -> Uri.t -> r
  = fun ~default l ->
    let rel, wl = build_info_list 1 l in
    let re = Re.(compile @@ whole_string @@ alt rel) in
    fun uri ->
      let s = prepare_uri uri in
      try
        let subs = Re.exec re s in
        find_and_trigger ~original:s subs wl
      with
          Not_found -> default uri
