open Path

(* www.bla.com/foo/%i/bla/%f?truc=%s *)
let u = rel/"foo"/%Int/"bla"/%Float/?("truc", List Int)**!("a", String)**nil
let u = finalize u Conv.id
let s = eval u

let uri = s 3 5. [1;2] "bla"

let () = Format.printf "%a\n%!" Uri.pp_hum uri


let u = host"www.foo.org"/%(List (Or(Int, String)))/?nil
let s = eval @@ finalize u
let uri = s @@ [R "bla"]

let () = Format.printf "%a\n%!" Uri.pp_hum uri
