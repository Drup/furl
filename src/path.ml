
(* Misc utilities *)

let map_snd f (x,y) = (x, f y)

let rev_intersperse sep l =
  let rec aux acc = function
    | []   -> acc
    | h::t -> aux (h::sep::acc) t
  in aux [] l

let find_idx el l =
  let rec aux el i = function
    | [] -> raise Not_found
    | x::l' ->
      if x == el then i
      else aux el (i+1) l'
  in aux el 0 l


let build_permutation l_before l_after =
  let t = Array.make (List.length l_before) 0 in
  l_before |> List.iteri (fun i x ->
    let j = find_idx x l_after in
    t.(i) <- j
  ) ;
  t


module Conv = struct

  type ('a, 'b) t = {
    of_ : 'b -> 'a;
    to_ : 'a -> 'b
  }

  let id =
    let id x = x in
    { of_ = id ; to_ = id }

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
  | Seq       : (nontop, 'a) atom * (nontop, 'b) atom -> (nontop, 'a * 'b) atom
  | Prefix    : string * (nontop, 'a) atom -> (nontop, 'a) atom
  | Suffix    : (nontop, 'a) atom * string -> (nontop, 'a) atom
  | List      : (nontop, 'a) atom -> (top, 'a list) atom
  | List1     : (nontop, 'a) atom -> (top, 'a * 'a list) atom

type ('ret, 'fu, 'retc, 'converter) query =
  | Nil  : ('r,'r,'rc, 'rc) query
  | Any  : ('r,'r,'rc, 'rc) query
  | Cons : (string * (_,'a) atom * ('r, 'f, 'rc, 'c) query)
    -> ('r, 'a -> 'f, 'rc, 'c) query
  | Conv : (string * (_,'a) atom * ('r, 'f, 'rc, 'c) query)
    -> ('r, 'b -> 'f, 'rc, ('a, 'b) Conv.t -> 'c) query

type ('return, 'fu, 'returnc, 'converter) path =
  | Host : string -> ('r,'r,'rc, 'rc) path
  | Rel : ('r,'r,'rc, 'rc) path
  | SuffixConst : (('r, 'f, 'rc, 'c) path * string)
    -> ('r, 'f, 'rc, 'c) path
  | SuffixAtom : (('a -> 'r, 'f, 'rc, 'c) path * (_,'a) atom)
    -> ('r, 'f, 'rc, 'c) path
  | SuffixConv : (('b -> 'r, 'f, ('a, 'b) Conv.t -> 'rc, 'c) path * (_,'a) atom)
    -> ('r, 'f, 'rc, 'c) path

type ('r, 'f, 'rc, 'c) conv_url =
  | Query :
      ( ('x, 'f, 'xc, 'c) path * ('r, 'x, 'rc, 'xc) query ) ->
    ('r, 'f, 'rc, 'c) conv_url
  | SlashQuery :
      ( ('x, 'f, 'xc, 'c) path * ('r, 'x, 'rc, 'xc) query ) ->
    ('r, 'f, 'rc, 'c) conv_url

let ( ** ) (n,x) y = Cons (n,x, y)
let ( **! ) (n,x) y = Conv (n,x,y)
let nil = Nil
let any = Any

let host x = Host x
let rel = Rel
let (/) a b = SuffixConst(a,b)
let (/%) a b = SuffixAtom(a,b)
let (/!) a b = SuffixConv(a,b)
let (/?) p q = Query (p,q)
let (//?) p q = SlashQuery (p,q)

(** {2 Proper urls} *)

(** {3 Utilities Pseudo-lists} *)

type (_,_) convlist =
  | Nil : ('a,'a) convlist
  | Conv : ('a, 'b) Conv.t * ('r, 'l) convlist -> ('r, ('a, 'b) Conv.t -> 'l) convlist

type (_,_) vonclist =
  | Nil : ('a, 'a) vonclist
  | Vonc : ('a, 'b) Conv.t * (('a, 'b) Conv.t -> 'r, 'l) vonclist -> ('r, 'l) vonclist

let rec rev_append
  : type c rc xc.
    (xc, c) vonclist -> (rc, xc) convlist -> (rc, c) convlist
  = fun l1 l2 -> match l1 with
    | Nil -> l2
    | Vonc (a,l) -> rev_append l (Conv(a,l2))



type ('r, 'f) url =
    Url : (('r,'f,('r,'f) url,'c) conv_url * (('r,'f) url, 'c) convlist) -> ('r,'f) url


(** {2 Finalization} *)

let rec finalize_path
  : type r f rc c.
    (r, f, rc, c) path ->
    ((rc,c) vonclist -> rc) -> c
  = fun p k -> match p with
    | Host _ -> k Nil
    | Rel    -> k Nil
    | SuffixConst (p, _) -> finalize_path p k
    | SuffixAtom  (p, _) -> finalize_path p k
    | SuffixConv  (p, _) ->
      finalize_path p (fun l c -> k (Vonc(c,l)))

let rec finalize_query
  : type r f rc c.
    (r, f, rc, c) query -> ((rc,c) convlist -> rc) -> c
  = fun p k -> match p with
    | Nil -> k Nil
    | Any -> k Nil
    | Cons (_,_,q) -> finalize_query q k
    | Conv (_,_,q) ->
      (fun c -> finalize_query q (fun l -> k (Conv (c,l))))

let finalize
  : type r f c. (r, f, (r,f) url, c) conv_url -> c
  = fun u ->
    let f p q =
      finalize_path p @@ fun lp ->
      finalize_query q @@ fun lq ->
      Url (u, rev_append lp lq)
    in match u with
      | Query (p,q)      -> f p q
      | SlashQuery (p,q) -> f p q

(** {2 Evaluation functions} *)

let rec eval_atom : type t a . (t,a) atom -> a -> string
  = function
  | Float  -> string_of_float
  | Int    -> string_of_int
  | Bool   -> string_of_bool
  | String -> (fun s -> s)
  | Opt p -> (function None -> "" | Some x -> eval_atom p x)
  | Seq (p1,p2) ->
    (fun (x1,x2) -> eval_atom p1 x1 ^ eval_atom p2 x2)
  | Prefix(s,p) ->
    fun x -> s ^ eval_atom p x
  | Suffix(p,s) ->
    fun x -> eval_atom p x ^ s
  | Or (pL, pR) ->
    (function L x -> eval_atom pL x | R x -> eval_atom pR x)
  | List p ->
    (fun l -> String.concat "" @@ List.map (eval_atom p) l)
  | List1 p ->
    (fun (x,l) -> String.concat "" @@ List.map (eval_atom p) @@ x::l)

let eval_top_atom : type t a . (t,a) atom -> a -> string list
  = function
  | Opt p -> (function None -> [] | Some x -> [eval_atom p x])
  | List p ->
    (fun l -> List.map (eval_atom p) l)
  | List1 p ->
    (fun (x,l) -> List.map (eval_atom p) @@ x::l)
  | e -> fun x -> [eval_atom e x]

let rec eval_path
  : type r f c xc.
    (r, f, xc, c) path ->
    ((_,_) url,c) convlist ->
    (((_,_) url, xc) convlist -> string option -> string list -> r) ->
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
    (r, f, xc, c) query ->
    ((_,_) url,c) convlist ->
    (((_,_) url, xc) convlist -> (string * string list) list -> r) ->
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

let eval_conv u l k =
  let aux ending_slash p q =
    eval_path p l @@ fun l host path ->
    eval_query q l @@ fun Nil query ->
    k @@
    let path =
      if ending_slash then "" :: path
      else path
    in Uri.make
      ?host
      ~path:(String.concat "/" @@ List.rev path)
      ~query ()
  in match u with
    | Query (p,q)      -> aux false p q
    | SlashQuery (p,q) -> aux true p q

let keval (Url (c,l)) k = eval_conv c l k

let eval url = keval url (fun x -> x)

(** {2 matching} *)

let slash = Re.char '/'
let comma = Re.char ','
let amper = Re.char '&'

type (_,_) re_atom =
  | Float     : (nontop, float) re_atom
  | Int       : (nontop, int) re_atom
  | Bool      : (nontop, bool) re_atom
  | String    : (nontop, string) re_atom
  | Opt       : Re.markid * (nontop, 'a) re_atom -> (_, 'a option) re_atom
  | Or        : Re.markid * (nontop, 'a) re_atom * Re.markid * (nontop,'b) re_atom -> (nontop, ('a,'b) sum) re_atom
  | Seq       : (nontop, 'a) re_atom * (nontop, 'b) re_atom -> (nontop, 'a * 'b) re_atom

  | List      : (nontop, 'a) re_atom * Re.re -> (top, 'a list) re_atom
  | List1     : (nontop, 'a) re_atom * Re.re -> (top, 'a * 'a list) re_atom

let rec re_atom
  : type t a. (t,a) atom -> (t,a) re_atom * Re.t
  = let open Re in function
    (* -?[0-9]+( .[0-9]* )? *)
    | Float       ->
      Float, group @@ seq [opt (char '-') ; rep1 digit ; opt (seq [char '.'; rep digit])]
    (* -?[0-9]+ *)
    | Int         -> Int, group @@ seq [opt (char '-') ; rep1 digit]
    (* true|false *)
    | Bool        -> Bool, group @@ alt [str "true" ; str "false"]
    (* [^/]+ *)
    | String      -> String, group @@ rep1 @@ compl [slash]
    | Opt e       ->
      let me, (id, re) = map_snd mark @@ re_atom e in
      Opt (id,me), alt [epsilon ; re]
    | Or (e1,e2)  ->
      let me1, (id1, re1) = map_snd mark @@ re_atom e1 in
      let me2, (id2, re2) = map_snd mark @@ re_atom e2 in
      Or (id1, me1, id2, me2), seq [alt [re1 ; re2]]
    | Prefix (s,e)->
      let me, re = re_atom e in
      me, seq [str s ; re]
    | Suffix (e,s)->
      let me, re = re_atom e in
      me, seq [re ; str s]
    | Seq (e1,e2) ->
      let me1, re1 = re_atom e1 in
      let me2, re2 = re_atom e2 in
      Seq (me1, me2), seq [re1; re2]

    (* top *)
    | List e      ->
      let me, re = re_atom e in
      List (me,Re.compile re), group @@ rep @@ no_group re
    | List1 e     ->
      let me, re = re_atom e in
      List1 (me,Re.compile re), group @@ rep1 @@ no_group re

let rec count_group
  : type t a. (t,a) re_atom -> int
  = function
    | Float  -> 1
    | Int  -> 1
    | Bool  -> 1
    | String  -> 1
    | Opt (_,e) -> count_group e
    | Or (_,e1,_,e2) -> count_group e1 + count_group e2
    | Seq (e1,e2) -> count_group e1 + count_group e2
    | List _ -> 1
    | List1 _ -> 1

let rec extract_list
  : type t a. (t,a) re_atom -> Re.re -> int -> Re.substrings -> a list
  = fun e re i s ->
    let aux s = snd @@ extract_atom e 1 s in
    let (pos, len) = Re.get_ofs s i in
    (* The whole original string*)
    let original = Re.get s 0 in
    Gen.to_list @@ Gen.map aux @@ Re.all_gen ~pos ~len re original

and extract_atom
  : type t a. (t,a) re_atom -> int -> Re.substrings -> int * a
  = fun x i s -> match x with
    | Float  -> i + 1, float_of_string (Re.get s i)
    | Int    -> i + 1, int_of_string   (Re.get s i)
    | Bool   -> i + 1, bool_of_string  (Re.get s i)
    | String -> i + 1, Re.get s i
    | Opt (id,e) ->
      if not @@ Re.marked s id then i + count_group e, None
      else map_snd (fun x -> Some x) @@ extract_atom e i s
    | Or (i1,e1,id2,e2) ->
      if Re.marked s i1 then
        map_snd (fun x -> L x) @@ extract_atom e1 i s
      else if Re.marked s id2 then
        map_snd (fun x -> R x) @@ extract_atom e2 (i+count_group e1) s
      else
        (* Invariant: Or produces [Re.alt [e1 ; e2]] *)
        assert false
    | Seq (e1,e2) ->
      let i, v1 = extract_atom e1 i s in
      let i, v2 = extract_atom e2 i s in
      i, (v1, v2)
    | List (e,re) -> i+1, extract_list e re i s
    | List1 (e,re) ->
      match extract_list e re i s with
        | h :: t -> i+1, (h, t)
        | _ ->
          (* Invariant: List1 produces [Re.rep1 e] *)
          assert false

let re_atom_path
  : type t a . (t,a) atom -> (t,a) re_atom * Re.t
  = let open Re in function
    | List  e ->
      let me, re = re_atom e in
      List (me, Re.compile re), group @@ rep @@ seq [slash; no_group re]
    | List1 e ->
      let me, re = re_atom e in
      List1 (me, Re.compile re), group @@ rep1 @@ seq [slash; no_group re]
    | Opt e ->
      let me, re = re_atom e in
      let id, re = mark re in
      Opt (id,me), seq [alt [epsilon ; seq [slash ; re]]]
    | e ->
      let me, re = re_atom e in
      me, seq [slash; re]

type (_,_,_,_) re_path =
  | Start : ('r,'r, 'rc, 'rc) re_path
  | SuffixConst : ('r, 'f, 'rc, 'c) re_path
    -> ('r, 'f, 'rc, 'c) re_path
  | SuffixAtom : (('a -> 'r, 'f, 'rc, 'c) re_path * (_,'a) re_atom)
    -> ('r, 'f, 'rc, 'c) re_path
  | SuffixConv : (('b -> 'r, 'f, ('a, 'b) Conv.t -> 'rc, 'c) re_path * (_,'a) re_atom)
    -> ('r, 'f, 'rc, 'c) re_path

let rec re_path
  : type r f rc fc .
    Re.t list -> (r,f,rc,fc) path -> (r,f,rc,fc) re_path * string option * Re.t list
  = let open Re in
  fun acc -> function
    | Host s            -> Start, Some s, acc
    | Rel               -> Start, None  , acc
    | SuffixConst (p,s) ->
      let (p,h,re) = re_path (slash :: str s :: acc) p in
      SuffixConst p, h, re
    | SuffixAtom (p,a) ->
      let ma, ra = re_atom_path a in
      let (p,h,re) = re_path (ra :: acc) p in
      SuffixAtom (p,ma), h, re
    | SuffixConv (p,a) ->
      let ma, ra = re_atom_path a in
      let (p,h,re) = re_path (ra :: acc) p in
      SuffixConv (p,ma), h, re

let re_atom_query
  : type t a . (t,a) atom -> (t,a) re_atom * Re.t
  = let open Re in function
    | List  e ->
      let me, re = re_atom e in
      let re' = no_group re in
      List (me, Re.compile re),
      group @@ alt [ epsilon ; seq [ re' ; rep @@ seq [comma; re']] ]
    | List1 e ->
      let me, re = re_atom e in
      let re' = no_group re in
      List1 (me, Re.compile re),
      group @@ seq [ re' ; rep @@ seq [comma; re']]
    | e ->
      let me, re = re_atom e in
      me, re

type ('ret, 'fu, 'retc, 'converter) re_query =
  | Nil  : ('r,'r,'rc, 'rc) re_query
  | Any  : ('r,'r,'rc, 'rc) re_query
  | Cons : (string * (_,'a) re_atom * ('r, 'f, 'rc, 'c) re_query)
    -> ('r, 'a -> 'f, 'rc, 'c) re_query
  | Conv : (string * (_,'a) re_atom * ('r, 'f, 'rc, 'c) re_query)
    -> ('r, 'b -> 'f, 'rc, ('a, 'b) Conv.t -> 'c) re_query

let rec re_query
  : type r f rc fc .
    (r,f,rc,fc) query -> (r,f,rc,fc) re_query * bool * (string * Re.t) list
  = function
    | Nil -> Nil, false, []
    | Any -> Any, true,  []
    | Cons (s,a,q) ->
      let ma, ra = re_atom_query a in
      let rq, b, l = re_query q in
      Cons (s, ma, rq), b, (s,ra) :: l
    | Conv (s,a,q) ->
      let ma, ra = re_atom_query a in
      let rq, b, l = re_query q in
      Conv (s, ma, rq), b, (s,ra) :: l


type ('r, 'f, 'rc, 'c) re_url =
  | Join :
      ( ('x, 'f, 'xc, 'c) re_path *
        ('r, 'x, 'rc, 'xc) re_query *
        int array
      ) -> ('r, 'f, 'rc, 'c) re_url



let re_conv_url
  : type r f rc fc . (r,f,rc,fc) conv_url -> (r,f,rc,fc) re_url * Re.t
  =

  let handle_path p rq t l =
    let (rp, host, re) = re_path l p in
    let re = match host with
      | Some host -> Re.str host :: re
      | None -> re
    in
    Join (rp, rq, t), Re.seq re
  in

  let aux
    : type r f x rc c xc .
      Re.t -> (x, f, xc, c) path ->  (r, x, rc, xc) query ->
      (r, f, rc, c) re_url * Re.t
    = fun path_sep p q -> match q with
      | Nil -> handle_path p Nil [||] []
      | Any ->
        let re = Re.(opt @@ seq [path_sep; rep any]) in
        handle_path p Nil [||] [re]
      | _ ->
        let (rq, any_query, reql) = re_query q in
        let rel =
          List.sort (fun (s1,_) (s2,_) -> compare (s1 : string) s2) reql
        in
        let t = build_permutation reql rel in

        let query_sep =
          if not any_query then amper
          else Re.(seq [amper; rep @@ seq [compl [amper] ; amper]])
        in
        let re =
          rel
          |> List.fold_left (fun l (s,re) ->
            re :: Re.str (s ^ "=") :: l
          ) []
          |> rev_intersperse query_sep
        in
        handle_path p rq t (path_sep :: re)
  in function
    | Query (p,q)      -> aux (Re.char '?') p q
    | SlashQuery (p,q) -> aux (Re.str "/?") p q
