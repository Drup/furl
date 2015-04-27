
let camlidae = Furl.Host "www.camlidae.ml"

let by_name () =
  Furl.(finalize @@ camlidae/"name"/%String/?Nil)

let by_humps () =
  Furl.(finalize @@ camlidae/"humps"/%Int/?Nil)

let by_humps () =
  Furl.(finalize @@ camlidae/"humps"/%Int/?("extinct",Opt Bool)**Nil)

type camlidae = {
  name : string ;
  humps : int ;
  vulgar_name : string ;
  extinct : bool
}

let list_camlidaes : camlidae list = []

let handle_camlidaes =
  Furl.match_url [
    Furl.ex (by_name ()) (fun n ->
      List.filter (fun c -> c.name = n) list_camlidaes
    ) ;
    Furl.ex (by_humps ()) (fun humps -> function
      | None ->
        List.filter (fun c -> c.humps = humps) list_camlidaes
      | Some b ->
        List.filter (fun c -> c.humps = humps && c.extinct = b) list_camlidaes
    );
  ]
    ~default:(fun _uri -> failwith "This is not a camlidae.")


let query_by_humps = Furl.eval @@ by_humps ()
