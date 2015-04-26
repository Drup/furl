open Furl_utils

module Converter = struct

  type ('a, 'b) t = {
    of_ : 'b -> 'a;
    to_ : 'a -> 'b
  }

  let id =
    let id x = x in
    { of_ = id ; to_ = id }

end

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

type ('fu, 'return, 'converter, 'returnc) query =
  | Nil  : ('r,'r, 'rc,'rc) query
  | Any  : ('r,'r, 'rc,'rc) query

  | Cons : string * ([`Top],'a) atom
      * (      'f, 'r, 'c, 'rc) query
     -> ('a -> 'f, 'r, 'c, 'rc) query

  | Conv : string * ([`Top],'a) atom
      * (      'f, 'r,                         'c, 'rc) query
     -> ('b -> 'f, 'r, ('a, 'b) Converter.t -> 'c, 'rc) query

type ('fu, 'return, 'converter, 'returnc) path =
  | Host : string -> ('r, 'r, 'rc, 'rc) path
  | Rel  : ('r, 'r ,'rc, 'rc) path
  | SuffixConst :
       ('f, 'r, 'c, 'rc) path * string
    -> ('f, 'r, 'c, 'rc) path
  | SuffixAtom :
       ('f,'a -> 'r, 'c, 'rc) path * ([`Top],'a) atom
    -> ('f,      'r, 'c, 'rc) path
  | SuffixConv :
       ('f, 'b -> 'r, 'c, ('a, 'b) Converter.t -> 'rc) path * ([`Top],'a) atom
    -> ('f,       'r, 'c,                         'rc) path

type slash = Slash | NoSlash

type ('f,'r,'c,'rc) url =
  | ConvUrl : slash
      * ('f, 'x,     'c, 'xc     ) path
      * (    'x, 'r,     'xc, 'rc) query
     -> ('f,     'r, 'c,      'rc) url

let ( ** ) (n,x) y = Cons (n,x, y)
let ( **! ) (n,x) y = Conv (n,x,y)
let nil = Nil
let any = Any

let host x = Host x
let rel = Rel
let (/) a b = SuffixConst(a,b)
let (/%) a b = SuffixAtom(a,b)
let (/!) a b = SuffixConv(a,b)
let (/?) p q = ConvUrl (NoSlash,p,q)
let (//?) p q = ConvUrl (Slash,p,q)



(** {2 Finalization} *)

(** Finalization is the act of gathering all the converters in a list
    and bundling it with a convertible url.

    We use a typed prepend list for this with two typed variables
    which correspond to 'c and 'rc from earlier.
*)

type (_,_) convlist =
  | Nil : ('a,'a) convlist
  | Conv : ('a, 'b) Converter.t
     * (                        'l, 'r) convlist
    -> (('a, 'b) Converter.t -> 'l, 'r) convlist

(** A finalized url is a convertible url and a list of converters.
    It simply quantifies existentially over the converters.

    Note the return type of the convlist: [('f, 'r) furl].
    We will see why very soon.
*)
type ('f, 'r) furl =
    Url :
        ('f, 'r, 'c, ('f, 'r) furl) url
      * (        'c, ('f, 'r) furl) convlist
     -> ('f, 'r ) furl

(**
   We want to construct the list of converters
   corresponding to the [c] type variable.

   We will construct two lists:
   - A prepend list for the queries: {!convlist}
   - A postpend list for the path: {!vonclist}
*)
type (_,_) vonclist =
  | Nil : ('a, 'a) vonclist
  | Vonc :  ('a, 'b) Converter.t
     * ('l, ('a, 'b) Converter.t -> 'r) vonclist
    -> ('l,                    'r) vonclist

(** We can write the statically typed rev_append.
    Notice how the type variables chain appropriately. *)
let rec rev_append
  : type c rc xc.
    (c, xc) vonclist -> (xc, rc) convlist -> (c, rc) convlist
  = fun l1 l2 -> match l1 with
    | Nil -> l2
    | Vonc (a,l) -> rev_append l (Conv(a,l2))


(** Our goal is to build the function that takes a convertible url,
    the list of convertible and returns a url.

    In practice: [('f, 'r, 'c, ('f, 'r) furl) url -> 'c].
    It justifies the return type of the conv list in {!url}.

    We proceed by CPS, passing the list of converters around
    while building a function to fill it.
*)
let rec finalize
  : type r f c. (f, r, c, (f, r) furl) url -> c
  = fun (ConvUrl (_, p, q) as url) ->
    finalize_path p @@ fun lp ->
    finalize_query q @@ fun lq ->
    Url (url, rev_append lp lq)

(** Once we have all these elements, it's simply type golf. *)

and finalize_path
  : type r f rc c.
    (f,r,c,rc) path -> ((c,rc) vonclist -> rc) -> c
  = fun p k -> match p with
    | Host _ -> k Nil
    | Rel    -> k Nil
    | SuffixConst (p, _) -> finalize_path p k
    | SuffixAtom  (p, _) -> finalize_path p k
    | SuffixConv  (p, _) ->
      finalize_path p (fun l c -> k (Vonc(c,l)))

and finalize_query
  : type r f rc c.
    (f,r,c,rc) query -> ((c,rc) convlist -> rc) -> c
  = fun p k -> match p with
    | Nil -> k Nil
    | Any -> k Nil
    | Cons (_,_,q) -> finalize_query q k
    | Conv (_,_,q) ->
      (fun c -> finalize_query q (fun l -> k (Conv (c,l))))


(** {2 Evaluation functions} *)

let rec eval_atom : type t a . (t,a) atom -> a -> string
  = function
  | Float  -> string_of_float
  | Int    -> string_of_int
  | Bool   -> string_of_bool
  | String -> (fun s -> s)
  (* TODO: We could potentially pre-compile the regexp. *)
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
    (fun l -> String.concat "" @@ List.map (eval_atom p) l)
  | List1 p ->
    (fun (x,l) -> String.concat "" @@ List.map (eval_atom p) @@ x::l)

let eval_top_atom : type a. ([`Top],a) atom -> a -> string list
  = function
  | Opt p -> (function None -> [] | Some x -> [eval_atom p x])
  | List p ->
    (fun l -> List.map (eval_atom p) l)
  | List1 p ->
    (fun (x,l) -> List.map (eval_atom p) @@ x::l)
  | e -> fun x -> [eval_atom e x]

let rec eval_path
  : type r f c xc.
    (f,r,c,xc) path ->
    (c, (_,_) furl) convlist ->
    ((xc, (_,_) furl) convlist -> string option -> string list -> r) ->
    f
  = fun p l k -> match p with
    | Host s -> k l (Some s) []
    | Rel -> k l None []
    | SuffixConst (p, s) ->
      eval_path p l @@ fun l h r -> k l h (s :: r)
    | SuffixAtom (p, a) ->
      eval_path p l @@ fun l h r x ->
      k l h (eval_top_atom a x @ r)
    | SuffixConv(p, a) as _p ->
      eval_path p l @@ fun l h r x ->
      let Conv(c, l) = l in
      k l h (eval_top_atom a (c.of_ x) @ r)

let rec eval_query
  : type r f c xc.
    (f,r,c,xc) query ->
    (c, (_,_) furl) convlist ->
    ((xc, (_,_) furl) convlist -> (string * string list) list -> r) ->
    f
  = fun q l k -> match q with
    | Nil -> k l []
    | Any -> k l []
    | Cons (n,a,q) ->
      fun x -> eval_query q l @@ fun l r ->
        k l ((n, eval_top_atom a x) :: r)
    | Conv (n,a,q) ->
      let Conv(c, l) = l in
      fun x -> eval_query q l @@ fun l r ->
          k l ((n, eval_top_atom a (c.of_ x)) :: r)

let keval (Url (ConvUrl(slash,p,q),cl)) k =
  eval_path p cl @@ fun cl host path ->
  eval_query q cl @@ fun Nil query ->
  k @@
  let path = match slash with
    | NoSlash -> "" :: path
    | Slash -> path
  in Uri.make
    ?host
    ~path:(String.concat "/" @@ List.rev path)
    ~query ()

let eval url = keval url (fun x -> x)

(** {2 matching} *)

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

let rec re_atom
  : type t a. component:_ -> (t,a) atom -> a re_atom * Re.t
  = let open Re in fun ~component -> function
    | Float       -> Float, group Furl_re.float
    | Int         -> Int, group Furl_re.arbitrary_int
    | Bool        -> Bool, group Furl_re.bool
    | String      -> String, group @@ Furl_re.string component
    | Regexp re   -> Regexp re, group @@ no_group re
    | Opt e       ->
      let me, (id, re) = map_snd mark @@ re_atom ~component e in
      Opt (id,me), alt [epsilon ; re]
    | Alt (e1,e2)  ->
      let me1, (id1, re1) = map_snd mark @@ re_atom ~component e1 in
      let me2, (id2, re2) = map_snd mark @@ re_atom ~component e2 in
      Alt (id1, me1, id2, me2), alt [re1 ; re2]
    | Prefix (s,e)->
      let me, re = re_atom ~component e in
      Nest me, seq [str s ; re]
    | Suffix (e,s)->
      let me, re = re_atom ~component e in
      Nest me, seq [re ; str s]
    | Seq (e1,e2) ->
      let me1, re1 = re_atom ~component e1 in
      let me2, re2 = re_atom ~component e2 in
      Seq (me1, me2), seq [re1; re2]

    (* top *)
    | List e      ->
      let me, re = re_atom ~component e in
      List (me,Re.compile re), group @@ rep @@ no_group re
    | List1 e     ->
      let me, re = re_atom ~component e in
      List1 (me,Re.compile re), group @@ rep1 @@ no_group re

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

let rec extract_atom
  : type a. a re_atom -> int -> Re.substrings -> int * a
  = fun rea i s -> match rea with
    | Float  -> incrg rea i, float_of_string (Re.get s i)
    | Int    -> incrg rea i, int_of_string   (Re.get s i)
    | Bool   -> incrg rea i, bool_of_string  (Re.get s i)
    | String -> incrg rea i, Re.get s i
    | Regexp _ -> incrg rea i, Re.get s i
    | Opt (id,e) ->
      if not @@ Re.marked s id then incrg rea i, None
      else map_snd (fun x -> Some x) @@ extract_atom e i s
    | Alt (i1,e1,id2,e2) ->
      if Re.marked s i1 then
        map_snd (fun x -> `Left x) @@ extract_atom e1 i s
      else if Re.marked s id2 then
        map_snd (fun x -> `Right x) @@ extract_atom e2 (incrg e1 i) s
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
        | _ ->
          (* Invariant: List1 produces [Re.rep1 e] *)
          assert false

and extract_list
  : type a. a re_atom -> Re.re -> int -> Re.substrings -> a list
  = fun e re i s ->
    let aux s = snd @@ extract_atom e 1 s in
    let (pos, pos') = Re.get_ofs s i in
    let len = pos' - pos in
    (* The whole original string*)
    let original = Re.get s 0 in
    Printf.eprintf "%s %i %i\n%!" original pos len ;
    Gen.to_list @@ Gen.map aux @@ Re.all_gen ~pos ~len re original


let re_atom_path
  : type a . (_,a) atom -> a re_atom * Re.t
  =
  let open Re in
  let component = `Path in
  function
    | List  e ->
      let me, re = re_atom ~component e in
      List (me, Re.compile re),
      group @@ Furl_re.list ~component 0 @@ no_group re
    | List1 e ->
      let me, re = re_atom ~component e in
      List1 (me, Re.compile re),
      group @@ Furl_re.list ~component 1 @@ no_group re
    | Opt e ->
      let me, re = re_atom ~component e in
      let id, re = mark re in
      Opt (id,me), seq [alt [epsilon ; seq [Furl_re.slash ; re]]]
    | e ->
      let me, re = re_atom ~component e in
      me, seq [Furl_re.slash; re]

type (_,_,_,_) re_path =
  | Start : ('r,'r, 'rc, 'rc) re_path
  | SuffixAtom :
       ('f, 'a -> 'r, 'c, 'rc) re_path * int * 'a re_atom
    -> ('f,       'r, 'c, 'rc) re_path
  | SuffixConv :
       ('f, 'b -> 'r, 'c, ('a, 'b) Converter.t -> 'rc) re_path
      * int * 'a re_atom
    -> ('f,       'r, 'c,                         'rc) re_path

let rec re_path
  : type r f rc fc .
    (f, r, fc, rc) path ->
    (f, r, fc, rc) re_path * int * Re.t list
  = let open Re in function
    | Host s ->
      let re = Re.str @@ Uri.pct_encode ~component:`Host s in
      Start, 1, [re]
    | Rel    -> Start, 1, []
    | SuffixConst (p,s) ->
      let (p, nb_group, re) = re_path p in
      p, nb_group, str s :: Furl_re.slash :: re
    | SuffixAtom (p,a) ->
      let ma, ra = re_atom_path a in
      let (p, nb_group, re) = re_path p in
      SuffixAtom (p, nb_group, ma), incrg ma nb_group, ra :: re
    | SuffixConv (p,a) ->
      let ma, ra = re_atom_path a in
      let (p, nb_group, re) = re_path p in
      SuffixConv (p, nb_group, ma), incrg ma nb_group, ra :: re


let re_atom_query
  : type a . (_,a) atom -> a re_atom * Re.t
  =
  let open Re in
  let component = `Query_value in
  function
    | List  e ->
      let me, re = re_atom ~component e in
      List (me, Re.compile re),
      group @@ Furl_re.list ~component 0 @@ no_group re
    | List1 e ->
      let me, re = re_atom ~component e in
      List1 (me, Re.compile re),
      group @@ Furl_re.list ~component 1 @@ no_group re
    | e ->
      let me, re = re_atom ~component e in
      me, re

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
    (f, r, fc, rc) query ->
    (f, r, fc, rc) re_query * bool * (string * Re.t * int) list
  = function
    | Nil -> Nil, false, []
    | Any -> Any, true,  []
    | Cons (s,a,q) ->
      let ma, ra = re_atom_query a in
      let rq, b, l = re_query q in
      Cons (ma, rq), b, (s, ra, count_group ma) :: l
    | Conv (s,a,q) ->
      let ma, ra = re_atom_query a in
      let rq, b, l = re_query q in
      Conv (ma, rq), b, (s,ra, count_group ma) :: l


type ('f,'r,'c,'rc) re_url =
  | ReUrl :
      ( ('f, 'x,    'c, 'xc     ) re_path *
        (    'x, 'r,    'xc, 'rc) re_query *
          int array
      ) -> ('f,'r,'c,'rc) re_url



let re_url
  : type r f rc fc . (r,f,rc,fc) url -> (r,f,rc,fc) re_url * Re.t
  = function ConvUrl(slash,p,q) ->
    let end_path = match slash with
      | NoSlash -> Re.epsilon
      | Slash -> Re.char '/'
    in
    let (rep, nb_group, repl) = re_path p in
    match q with
      | Nil ->
        ReUrl (rep, Nil, [||]),
        Re.seq @@ List.rev (end_path :: repl)

      | Any ->
        let end_re = Re.(opt @@ seq [Re.char '?' ; rep any]) in
        ReUrl (rep, Nil, [||]),
        Re.seq @@ List.rev_append repl [end_path; end_re]

      | _ ->
        let (req, any_query, reql) = re_query q in
        let rel =
          List.sort (fun (s1,_,_) (s2,_,_) -> compare (s1 : string) s2) reql
        in
        let t =
          build_permutation nb_group (fun (_,_,i) -> i) reql rel
        in

        let query_sep = Furl_re.query_sep any_query in
        let add_around_query =
          if not any_query then fun x -> x
          else fun l -> Re.(rep any) :: l
        in

        let re =
          rel
          |> List.fold_left (fun l (s,re,_) ->
            Re.seq [Re.str (s ^ "=") ; re ] :: l
          ) []
          |> intersperse query_sep
          |> add_around_query
          |> List.rev
          |> add_around_query
        in
        ReUrl(rep,req,t),
        Re.seq @@ List.rev_append repl (end_path :: Re.char '?' :: re)




let rec extract_path
  : type f x r c xc xc'.
    (f,x,c,xc) re_path ->
    ((xc, (_, _) furl) convlist -> x -> r * (xc', (_, _) furl) convlist) ->
    Re.substrings ->
    (c, (_, _) furl) convlist ->
    f -> r * (xc', (_, _) furl) convlist
  = fun rp k subs cl -> match rp with
    | Start  -> k cl
    | SuffixAtom (rep, idx, rea) ->
      let _, v = extract_atom rea idx subs in
      let k cl f = k cl (f v) in
      extract_path rep k subs cl
    | SuffixConv (rep, idx, rea) ->
      let _, v = extract_atom rea idx subs in
      let k (cl : _ convlist) f =
        let Conv (conv,cl) = cl in
        k cl (f @@ conv.to_ v)
      in
      extract_path rep k subs cl


let rec extract_query
  : type x r xc rc.
    (x,r,xc,rc) re_query ->
    int -> Re.substrings -> int array ->
    (xc, (_, _) furl) convlist ->
    x -> r * (rc, (_, _) furl) convlist
  = fun rq i subs permutation cl f -> match rq with
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
    (f, r, fc, (_, _) furl) re_url ->
    (fc, (_, _) furl) convlist ->
    Re.substrings -> f -> r
  = fun (ReUrl (rp, rq, permutation)) cl subs f ->
    let k = extract_query rq 0 subs permutation in
    let r, Nil = extract_path rp k subs cl f in
    r

let get_re
  : type r f. (f, r) furl -> Re.t
  = function Url (url, _) ->
    snd @@ re_url url

let extract
  : type r f. (f, r) furl -> f:f -> Uri.t -> r
  = function Url (conv_url,cl) ->
    let re_url, re = re_url conv_url in
    let re = Re.(compile @@ whole_string re) in
    fun ~f uri ->
      let uri =
        Uri.with_query uri @@
        List.sort (fun (x,_) (y,_) -> compare (x: string) y) @@
        Uri.query uri
      in
      let s = Uri.path_and_query uri in
      let subs = Re.exec re s in
      extract_url re_url cl subs f
