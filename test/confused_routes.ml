let a i j () = Format.printf ">>> a %i, %i" i j
let b i j () = Format.printf ">>> b %i, %i" i j
let default uri () = Format.printf ">>> default: %S" (Uri.to_string uri)

let routes =
  let open Furl in [
    (rel / "a" /% Tyre.int /? ("x", Tyre.int) ** nil) --> a ;
    (rel / "b" /% Tyre.int /? ("x", Tyre.int) ** nil) --> b ;
  ]

let () =
  Furl.match_url ~default routes (Uri.of_string Sys.argv.(1)) ()
