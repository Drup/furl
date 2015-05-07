
let camlidae () = Furl.host "www.camlidae.ml"

let by_name () =
  Furl.(finalize @@ ~$camlidae / "name" /% String /? nil)

let by_humps () =
  Furl.(finalize @@ ~$camlidae / "humps" /% Int /? ("extinct",Opt Bool) ** nil)

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
        ~$by_name  --> handle_by_name ;
        ~$by_humps --> handle_by_hump ;
      ])

let query_by_humps = Furl.eval @@ by_humps ()
