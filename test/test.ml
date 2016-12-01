open Tyre

let string = regex Re.(rep1 @@ compl [char ','])

(* www.bla.com/foo/%i/bla/%f?truc=%s *)
let url () =
  Furl.(
    rel/"foo"/%int/"bla"/%float/?("truc", list int)**("a", string)**nil
  )

let uri = Furl.(eval ~$url) 3 5. [1;2] "bla"

let () =
  Format.printf "%a\n%!" Uri.pp_hum uri

let url () =
  Furl.(
    rel/"foo"/%int/% (float <&> list float) //?("foo",bool)**("bla",string)**any
  )
let uri = Furl.(eval ~$url) 3 (4.,[]) true "hello"

let () =
  Format.printf "%a\n%!" Uri.pp_hum uri ;

  Furl.(extract ~$url)
    (Uri.of_string "/foo/3/4./?foo=false&athing&bla=x")
    ~f:(fun i (f,fl) b s ->
      Format.printf "%d (%a) %s %b"
        i
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
           Format.pp_print_float) (f::fl)
        s
        b
    )
