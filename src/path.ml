
type ('a, 'b) conv = {
  of_ : 'b -> 'a;
  to_ : 'a -> 'b
}

let id x = x

let conv_id = { of_ = id ; to_ = id }

type ('a,'b) sum = L of 'a | R of 'b

type _ atom =
  | Float     : float atom
  | Int       : int atom
  | Int32     : int32 atom
  | Int64     : int64 atom
  | Nativeint : nativeint atom
  | Bool      : bool atom
  | String    : string atom
  | Opt       : 'a atom -> 'a option atom
  | Or        : 'a atom * 'b atom -> ('a,'b) sum atom
  | Star      : 'a atom -> 'a list atom
  | Plus      : 'a atom -> ('a * 'a list) atom
  | Seq       : 'a atom * 'b atom -> ('a * 'b) atom

type ('ret, 'fu, 'retc, 'converter) query =
  | Nil  : ('r,'r,'rc, 'rc) query
  | Cons : (string * 'a atom * ('r, 'f, 'rc, 'c) query)
    -> ('r, 'a -> 'f, 'rc, 'c) query
  | Conv : (string * 'a atom * ('r, 'f, 'rc, 'c) query)
    -> ('r, 'b -> 'f, 'rc, ('a, 'b) conv -> 'c) query

let ( ** ) (n,x) y = Cons (n,x, y)
let ( **? ) (n,x) y = Conv (n,x,y)
let nil = Nil


type ('return, 'fu, 'returnc, 'converter) path =
  | Host : string -> ('r,'r,'rc, 'rc) path
  | Rel : ('r,'r,'rc, 'rc) path
  | Suffix : (('r, 'f, 'rc, 'c) path * string)
    -> ('r, 'f, 'rc, 'c) path
  | SuffixAtom : (('a -> 'r, 'f, 'rc, 'c) path * 'a atom)
    -> ('r, 'f, 'rc, 'c) path
  | SuffixConv : (('b -> 'r, 'f, ('a, 'b) conv -> 'rc, 'c) path * 'a atom)
    -> ('r, 'f, 'rc, 'c) path


let host x = Host x
let (/) a b = Suffix(a,b)
let (/!) a b = SuffixAtom(a,b)
let (//!) a b = SuffixConv(a,b)
let (~/) x = Rel/x


type (_,_) convlist =
  | Nil : ('a,'a) convlist
  | Conv : ('a, 'b) conv * ('r, 'l) convlist -> ('r, ('a, 'b) conv -> 'l) convlist

type (_,_) vonclist =
  | Nil : ('a, 'a) vonclist
  | Vonc : ('a, 'b) conv * (('a, 'b) conv -> 'r, 'l) vonclist -> ('r, 'l) vonclist

let rec vonc_conv
  : type c rc xc.
    (xc, c) vonclist -> (rc, xc) convlist -> (rc, c) convlist
  = fun l1 l2 -> match l1 with
    | Nil -> l2
    | Vonc (a,l) -> vonc_conv l @@ Conv(a,l2)


type ('r, 'f, 'rc, 'c) conv_uri =
  | Conv_uri :
      ( ('x, 'f, 'xc, 'c) path * ('r, 'x, 'rc, 'xc) query ) ->
    ('r, 'f, 'rc, 'c) conv_uri

let (/?) p q = Conv_uri (p,q)

type ('r, 'f) uri =
    Uri : (('r,'f,('r,'f) uri,'c) conv_uri * (('r,'f) uri, 'c) convlist) -> ('r,'f) uri



(** {2 Finalization} *)

let rec finalize_path
  : type r f rc c.
    (r, f, rc, c) path ->
    ((rc,c) vonclist -> rc) -> c
  = fun p k -> match p with
    | Host _ -> k Nil
    | Rel    -> k Nil
    | Suffix (p,_) -> finalize_path p k
    | SuffixAtom (p, _) -> finalize_path p k
    | SuffixConv (p, _) ->
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
  : type r f c. (r, f, (r,f) uri, c) conv_uri -> c
  = fun (Conv_uri (p,q) as u) ->
    finalize_path p @@ fun lp ->
    finalize_query q @@ fun lq ->
    Uri (u, vonc_conv lp lq)

(** {2 Evaluation functions} *)

let rec eval_atom : type a . a atom -> a -> string list
  = function
  | Float  -> (fun x -> [string_of_float x])
  | Int    -> (fun x -> [string_of_int x])
  | Int32  -> (fun x -> [Int32.to_string x])
  | Int64  -> (fun x -> [Int64.to_string x])
  | Nativeint -> (fun x -> [Nativeint.to_string x])
  | Bool   -> (fun x -> [string_of_bool x])
  | String -> (fun s -> [s])
  | Opt p -> (function None -> [] | Some x -> eval_atom p x)
  | Or (pL, pR) ->
    (function L x -> eval_atom pL x | R x -> eval_atom pR x)
  | Star p ->
    (fun l -> List.concat @@ List.map (eval_atom p) l)
  | Plus p ->
    (fun (x,l) -> List.concat @@ List.map (eval_atom p) @@ x::l)
  | Seq (p1,p2) ->
    (fun (x1,x2) -> eval_atom p1 x1 @ eval_atom p2 x2)

let rec eval_path
  : type r f c xc.
    (r, f, xc, c) path ->
    ((_,_) uri,c) convlist ->
    (((_,_) uri, xc) convlist -> string option -> string list -> r) ->
    f
  = fun p l k -> match p with
    | Host s -> k l (Some s) []
    | Rel -> k l None []
    | Suffix (p, s) ->
      eval_path p l @@ fun l h r -> k l h (s :: r)
    | SuffixAtom (p, a) ->
      eval_path p l @@ fun l h r x ->
      k l h (eval_atom a x @ r)
    | SuffixConv(p, a) as _p ->
      eval_path p l @@ fun l h r x ->
      let Conv(c, l) = l in
      k l h (eval_atom a (c.of_ x) @ r)

let rec eval_query
  : type r f c xc.
    (r, f, xc, c) query ->
    ((_,_) uri,c) convlist ->
    (((_,_) uri, xc) convlist -> (string * string list) list -> r) ->
    f
  = fun q l k -> match q with
    | Nil -> k l []
    | Cons (n,a,q) ->
      fun x -> eval_query q l @@ fun l r ->
        k l ((n, eval_atom a x) :: r)
    | Conv (n,a,q) ->
      let Conv(c, l) = l in
      fun x -> eval_query q l @@ fun l r ->
          k l ((n, eval_atom a (c.of_ x)) :: r)

let eval_conv (Conv_uri (p,q)) l =
  eval_path p l @@ fun l host path ->
  eval_query q l @@ fun Nil query ->
  Uri.make
    ?host
    ~path:(String.concat "/" @@ List.rev path)
    ~query ()

let eval (Uri (c,l)) = eval_conv c l




(* www.bla.com/foo/%i/bla/%f?truc=%s *)
let u = ~/"foo"/!Int/"bla"/!Float/?("truc", Star Int)**?("a", String)**Nil
let u = finalize u conv_id
let s = eval u

let uri = s 3 5. [1;2] "bla"

let () = Format.printf "%a" Uri.pp_hum uri
