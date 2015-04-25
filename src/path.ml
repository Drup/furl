
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
  | List      : (nontop, 'a) atom -> (top, 'a list) atom
  | List1     : (nontop, 'a) atom -> (top, 'a * 'a list) atom
  | Seq       : (nontop, 'a) atom * (nontop, 'b) atom -> (nontop, 'a * 'b) atom
  | Prefix    : string * (nontop, 'a) atom -> (nontop, 'a) atom
  | Suffix    : (nontop, 'a) atom * string -> (nontop, 'a) atom

type ('ret, 'fu, 'retc, 'converter) query =
  | Nil  : ('r,'r,'rc, 'rc) query
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

type (_,_) re_atom =
  | Float     : (nontop, float) re_atom
  | Int       : (nontop, int) re_atom
  | Bool      : (nontop, bool) re_atom
  | String    : (nontop, string) re_atom
  | Opt       : Re.markid * (nontop, 'a) re_atom -> (_, 'a option) re_atom
  | Or        : Re.markid * (nontop, 'a) re_atom * Re.markid * (nontop,'b) re_atom -> (nontop, ('a,'b) sum) re_atom
  | Seq       : (nontop, 'a) re_atom * (nontop, 'b) re_atom -> (nontop, 'a * 'b) re_atom
  | Prefix    : string * (nontop, 'a) re_atom -> (nontop, 'a) re_atom
  | Suffix    : (nontop, 'a) re_atom * string -> (nontop, 'a) re_atom

  | List      : (nontop, 'a) re_atom -> (top, 'a list) re_atom
  | List1     : (nontop, 'a) re_atom -> (top, 'a * 'a list) re_atom

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
      let me, re = re_atom e in
      let id, re = mark re in
      Opt (id,me), alt [epsilon ; re]
    | Or (e1,e2)  ->
      let me1, re1 = re_atom e1 in
      let id1, re1 = mark re1 in
      let me2, re2 = re_atom e2 in
      let id2, re2 = mark re2 in
      Or (id1, me1, id2, me2), seq [alt [re1 ; re2]]
    | Prefix (s,e)->
      let me, re = re_atom e in
      Prefix (s,me), seq [str s ; re]
    | Suffix (e,s)->
      let me, re = re_atom e in
      Suffix (me,s), seq [re ; str s]
    | Seq (e1,e2) ->
      let me1, re1 = re_atom e1 in
      let me2, re2 = re_atom e2 in
      Seq (me1, me2), seq [re1; re2]

    (* top *)
    | List e      ->
      let me, re = re_atom e in
      List me, group @@ rep @@ no_group re
    | List1 e     ->
      let me, re = re_atom e in
      List1 me, group @@ rep1 @@ no_group re

let re_atom_path
  : type t a . (t,a) atom -> (t,a) re_atom * Re.t
  = let open Re in function
    | List  e ->
      let me, re = re_atom e in
      List me, group @@ rep @@ seq [slash; no_group re]
    | List1 e ->
      let me, re = re_atom e in
      List1 me, group @@ rep1 @@ seq [slash; no_group re]
    | Opt e       ->
      let me, re = re_atom e in
      let id, re = mark re in
      Opt (id,me), seq [alt [epsilon ; seq [slash ; re]]]
    | e ->
      let me, re = re_atom e in
      me, seq [slash; re]

let re_atom_query
  : type t a . (t,a) atom -> (t,a) re_atom * Re.t
  = let open Re in function
    | List  e ->
      let me, re = re_atom e in
      let re = no_group re in
      List me, group @@ alt [ epsilon ; seq [ re ; rep @@ seq [comma; re]] ]
    | List1 e ->
      let me, re = re_atom e in
      let re = no_group re in
      List1 me, group @@ seq [ re ; rep @@ seq [comma; re]]
    | e ->
      let me, re = re_atom e in
      me, re


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

type ('ret, 'fu, 'retc, 'converter) re_query =
  | Nil  : ('r,'r,'rc, 'rc) re_query
  | Cons : (string * (_,'a) re_atom * ('r, 'f, 'rc, 'c) re_query)
    -> ('r, 'a -> 'f, 'rc, 'c) re_query
  | Conv : (string * (_,'a) re_atom * ('r, 'f, 'rc, 'c) re_query)
    -> ('r, 'b -> 'f, 'rc, ('a, 'b) Conv.t -> 'c) re_query

let rec re_query
  : type r f rc fc .
    (r,f,rc,fc) query -> (r,f,rc,fc) re_query * (string * Re.t) list
  = function
    | Nil -> Nil, []
    | Cons (s,a,q) ->
      let ma, ra = re_atom_path a in
      let rq, l = re_query q in
      Cons (s, ma, rq), (s,ra) :: l
    | Conv (s,a,q) ->
      let ma, ra = re_atom_path a in
      let rq, l = re_query q in
      Conv (s, ma, rq), (s,ra) :: l

let re_conv_url
  : type r f rc fc . (r,f,rc,fc) conv_url -> Re.t list
  = function
    | Query (p,q) -> failwith "TODO"
    | SlashQuery (p,q) -> failwith "TODO"
