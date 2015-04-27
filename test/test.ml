open Furl

(* www.bla.com/foo/%i/bla/%f?truc=%s *)
let raw_u = Rel/"foo"/%Int/"bla"/%Float/?("truc", List Int)**("a", String)**Nil
let url = finalize raw_u

let uri = eval url 3 5. [1;2] "bla"

let () =
  Format.printf "%a\n%!" Uri.pp_hum uri

let raw_u () =
  Rel/"foo"/%Int/%List1 Float//?("foo",Bool)**("bla",String)**Any
let url () = finalize @@ raw_u ()
let uri = eval (url ()) 3 (4.,[]) true "hello"

let () =
  Format.printf "%a\n%!" Uri.pp_hum uri ;

  extract (url ())
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
