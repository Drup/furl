(** A simple url accepting some json stuff in query parameters. *)

let json_converter = Furl.Converter.{
    to_ = (fun s -> Yojson.Safe.from_string s) ;
    of_ = (fun s -> Yojson.Safe.to_string s)   ;
  }

(* The only hole of this parameter is a Json object, converted by
   json_converter from/to strings.
*)
let my_json_url () =
  Furl.(finalize @@ rel /? ("json", String) **! nil )
    json_converter
