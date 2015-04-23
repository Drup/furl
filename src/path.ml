
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
  | List      : 'a atom -> 'a list atom
  | List1     : 'a atom -> ('a * 'a list) atom
  | Seq       : 'a atom * 'b atom -> ('a * 'b) atom
  | Prefix    : string * 'a atom -> 'a atom
  | Suffix    : 'a atom * string -> 'a atom

type ('ret, 'fu, 'retc, 'converter) query =
  | Nil  : ('r,'r,'rc, 'rc) query
  | Cons : (string * 'a atom * ('r, 'f, 'rc, 'c) query)
    -> ('r, 'a -> 'f, 'rc, 'c) query
  | Conv : (string * 'a atom * ('r, 'f, 'rc, 'c) query)
    -> ('r, 'b -> 'f, 'rc, ('a, 'b) Conv.t -> 'c) query

type ('return, 'fu, 'returnc, 'converter) path =
  | Host : string -> ('r,'r,'rc, 'rc) path
  | Rel : ('r,'r,'rc, 'rc) path
  | SuffixConst : (('r, 'f, 'rc, 'c) path * string)
    -> ('r, 'f, 'rc, 'c) path
  | SuffixAtom : (('a -> 'r, 'f, 'rc, 'c) path * 'a atom)
    -> ('r, 'f, 'rc, 'c) path
  | SuffixConv : (('b -> 'r, 'f, ('a, 'b) Conv.t -> 'rc, 'c) path * 'a atom)
    -> ('r, 'f, 'rc, 'c) path

type ('r, 'f, 'rc, 'c) conv_uri =
  | Query :
      ( ('x, 'f, 'xc, 'c) path * ('r, 'x, 'rc, 'xc) query ) ->
    ('r, 'f, 'rc, 'c) conv_uri
  | SlashQuery :
      ( ('x, 'f, 'xc, 'c) path * ('r, 'x, 'rc, 'xc) query ) ->
    ('r, 'f, 'rc, 'c) conv_uri

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

(** {2 Proper uris} *)

(** {3 Utilities Pseudo-lists} *)

type (_,_) convlist =
  | Nil : ('a,'a) convlist
  | Conv : ('a, 'b) Conv.t * ('r, 'l) convlist -> ('r, ('a, 'b) Conv.t -> 'l) convlist

type (_,_) vonclist =
  | Nil : ('a, 'a) vonclist
  | Vonc : ('a, 'b) Conv.t * (('a, 'b) Conv.t -> 'r, 'l) vonclist -> ('r, 'l) vonclist

let rec rec_append
  : type c rc xc.
    (xc, c) vonclist -> (rc, xc) convlist -> (rc, c) convlist
  = fun l1 l2 -> match l1 with
    | Nil -> l2
    | Vonc (a,l) -> rec_append l (Conv(a,l2))



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
  : type r f c. (r, f, (r,f) uri, c) conv_uri -> c
  = fun u ->
    let f p q =
      finalize_path p @@ fun lp ->
      finalize_query q @@ fun lq ->
      Uri (u, rec_append lp lq)
    in match u with
      | Query (p,q)      -> f p q
      | SlashQuery (p,q) -> f p q


(** {2 Evaluation functions} *)

let rec append_last s = function
  | [] -> [s]
  | [h] -> [h^s]
  | h :: t -> h :: append_last s t

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
  | List p ->
    (fun l -> List.concat @@ List.map (eval_atom p) l)
  | List1 p ->
    (fun (x,l) -> List.concat @@ List.map (eval_atom p) @@ x::l)
  | Seq (p1,p2) ->
    (fun (x1,x2) -> eval_atom p1 x1 @ eval_atom p2 x2)
  | Prefix(s,p) ->
    fun x -> begin match eval_atom p x with
        | [] -> [s]
        | h :: t -> (s^h) :: t
      end
  | Suffix(p,s) ->
    fun x -> append_last s @@ eval_atom p x


let rec eval_path
  : type r f c xc.
    (r, f, xc, c) path ->
    ((_,_) uri,c) convlist ->
    (((_,_) uri, xc) convlist -> string option -> string list -> r) ->
    f
  = fun p l k -> match p with
    | Host s -> k l (Some s) []
    | Rel -> k l None []
    | SuffixConst (p, s) ->
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

let keval (Uri (c,l)) k = eval_conv c l k

let eval uri = keval uri (fun x -> x)

(** {2 matching} *)

let slash = Re.char '/'

let interleave l =
  let rec aux acc = function
    | []   -> List.rev acc
    | h::t -> aux (h::slash::acc) t
  in aux [] l

let rec regexp_atom_list
  : type a . a atom -> Re.t list
  = let open Re in function
    (* -?[0-9]+( .[0-9]* )? / *)
    | Float       ->
      [opt (char '-') ; rep1 digit ; opt (seq [char '.'; rep digit])]
    (* -?[0-9]+ / *)
    | Int         -> [opt (char '-') ; rep1 digit]
    | Int32       -> [opt (char '-') ; rep1 digit]
    | Int64       -> [opt (char '-') ; rep1 digit]
    | Nativeint   -> [opt (char '-') ; rep1 digit]
    (* true|false / *)
    | Bool        -> [alt [str "true" ; str "false"]]
    (* [^/]+ / *)
    | String      -> [rep1 @@ compl [slash]]
    | Opt e       -> [alt [epsilon ; regexp_atom e]]
    | Or (e1,e2)  ->
      [alt [regexp_atom e1 ; regexp_atom e2]]
    | List e      -> [rep @@ regexp_atom e]
    | List1 e     -> [rep1 @@ regexp_atom e]
    | Seq (e1,e2) -> regexp_atom_list e1 @ regexp_atom_list e2
    | Prefix (s,e)-> str s :: regexp_atom_list e
    | Suffix (e,s)-> regexp_atom_list e @ [str s]

and regexp_atom
  : type a . a atom -> Re.t
  = fun l -> Re.seq @@ interleave @@ regexp_atom_list l

let rec regexp_path
  : type r f rc fc . (r,f,rc,fc) path -> Re.t list
  = function
    | Host _            -> []
    | Rel               -> []
    | SuffixConst (p,s) -> regexp_path p @ [slash; Re.str s]
    | SuffixAtom  (p,a) -> regexp_path p @ (slash :: regexp_atom_list a)
    | SuffixConv  (p,a) -> regexp_path p @ (slash :: regexp_atom_list a)

let regexp_conv_uri
  : type r f rc fc . (r,f,rc,fc) conv_uri -> Re.t list
  = function
    | Query (p,q) -> failwith "TODO"
    | SlashQuery (p,q) -> failwith "TODO"
