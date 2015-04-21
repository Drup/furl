
type ('a, 'b) conv = {
  of_ : 'b -> 'a;
  to_ : 'a -> 'b
}

type ('a,'b) sum = L of 'a | R of 'b

type coordinates =
  { abscissa: int ;
    ordinate: int }

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
  | Plus      : 'a atom -> 'a list atom
  | Seq       : 'a atom * 'b atom -> ('a * 'b) atom
  | Const     : 'a atom

let rec string_of_atom : type a . a atom -> a -> string
  = function
  | Float  -> string_of_float
  | Int    -> string_of_int
  | Int32  -> Int32.to_string
  | Int64  -> Int64.to_string
  | Nativeint -> Nativeint.to_string
  | Bool   -> string_of_bool
  | String -> (fun s -> s)
  | Opt p -> (function None -> "" | Some x -> string_of_atom p x)

let rec atom_of_string : type a . a atom -> string -> a
  = function
  | Float  -> float_of_string
  | Int    -> int_of_string
  | Int32  -> Int32.of_string
  | Int64  -> Int64.of_string
  | Nativeint -> Nativeint.of_string
  | Bool   -> bool_of_string
  | String -> (fun s -> s)
  | Opt x -> (function "" -> None | s -> Some (atom_of_string x s))


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
  | SuffixAtom : (('r, 'f, 'rc, 'c) path * 'a atom)
    -> ('r, 'a -> 'f, 'rc, 'c) path
  | SuffixConv : (('r, 'f, 'rc, 'c) path * 'a atom)
    -> ('r, 'b -> 'f, 'rc, ('a, 'b) conv -> 'c) path


let host x = Host x
let (/) a b = Suffix(a,b)
let (/!) a b = SuffixAtom(a,b)
let (//!) a b = SuffixConv(a,b)


type (_,_) convlist =
  | Nil : (unit,unit) convlist
  | Conv : ('a, 'b) conv * ('r, 'l) convlist -> ('r, ('a, 'b) conv -> 'l) convlist

type ('r, 'f, 'rc, 'c) conv_uri =
  | Conv_uri :
      ( ('x, 'f, 'xc, 'c) path * ('r, 'x, 'rc, 'xc) query ) ->
    ('r, 'f, 'rc, 'c) conv_uri

let (/?) p q = Conv_uri (p,q)

type ('r, 'f) uri =
    Uri : (('r,'f,unit,'c) conv_uri * (unit, 'c) convlist) -> ('r,'f) uri

let rec eval_path
  : type r f rc c xc.
    (r, f, xc, c) path ->
    (rc,c) convlist ->
    ((rc, xc) convlist -> string option -> string list -> r) ->
    f
  = fun p l k -> match p with
    | Host s -> k l (Some s) []
    | Rel -> k l None []
    | Suffix (p, s) ->
      eval_path p l @@ fun l h r -> k l h (s :: r)
    | SuffixAtom (p, a) ->
      fun x -> eval_path p l @@ fun l h r ->
        k l h (string_of_atom a x :: r)
    | SuffixConv(p, a) as _p ->
      let Conv(c, l) = l in
      fun x -> eval_path p l @@ fun l h r ->
        k l h (string_of_atom a (c.of_ x) :: r)

let rec eval_query
  : type r f rc c xc.
    (r, f, xc, c) query ->
    (rc,c) convlist ->
    ((rc, xc) convlist -> (string * string) list -> r) ->
    f
  = fun q l k -> match q with
    | Nil -> k l []
    | Cons (n,a,q) ->
      fun x -> eval_query q l @@ fun l r ->
        k l ((n, string_of_atom a x) :: r)
    | Conv (n,a,q) ->
      let Conv(c, l) = l in
      fun x -> eval_query q l @@ fun l r ->
          k l ((n, string_of_atom a (c.of_ x)) :: r)

let eval_conv
  : type f c . (Uri.t,f,unit,c) conv_uri -> (unit, c) convlist -> f
  = fun (Conv_uri (p,q)) l ->
    eval_path p l @@ fun l host path ->
    eval_query q l @@ fun Nil query ->
    Uri.make
      ?host
      ~path:(String.concat "/" path)
      ~query:(List.map (fun (n,k) -> (n,[k])) query) ()

let eval (Uri (c,l)) = eval_conv c l

(* www.bla.com/foo/%i/bla/%f?truc=%i+ *)
let u = host"www.bla.com"/"foo"/!Int/"bla"/!Float/?
    ("truc", Int)**("a", String)**Nil
let s = eval_conv u Nil 3. 1 2 "bla"

let () = Format.printf "%a" Uri.pp_hum s
