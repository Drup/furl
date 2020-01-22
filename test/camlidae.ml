open Tyre

let string = regex Re.(rep1 @@ compl [char '/'])

let camlidae = Furl.host "www.camlidae.ml"

let by_name =
  Furl.(camlidae / "name" /% string /? nil)

let by_humps =
  Furl.(camlidae / "humps" /% int /? ("extinct",opt bool) ** nil)

type camlidae = {
  name : string ;
  humps : int ;
  extinct : bool
}

let list_camlidaes = [
  { name="Bactrian camel" ; humps=2 ; extinct=false } ;
  { name="Dromedary"      ; humps=1 ; extinct=false } ;
  { name="Wild camel"     ; humps=2 ; extinct=false } ;
  { name="Llama"          ; humps=0 ; extinct=false } ;
  { name="Camelops"       ; humps=1 ; extinct=true  } ;
]

let handle_by_name n = List.filter (fun c -> c.name = n) list_camlidaes

let handle_by_hump humps = function
  | None ->
    List.filter (fun c -> c.humps = humps) list_camlidaes
  | Some b ->
    List.filter (fun c -> c.humps = humps && c.extinct = b) list_camlidaes

let handle_camlidaes =
  Furl.(match_url
      ~default:(fun _uri -> failwith "This is not a camlidae.")
      [
        by_name  --> handle_by_name ;
        by_humps --> handle_by_hump ;
      ])

let query_by_humps = Furl.eval @@ by_humps

let uri = query_by_humps 1 None

let s = handle_camlidaes uri
