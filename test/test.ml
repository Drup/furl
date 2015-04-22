open Path

(* www.bla.com/foo/%i/bla/%f?truc=%s *)
let u = rel/"foo"/%Int/"bla"/%Float/?("truc", Star Int)**!("a", String)**nil
let u = finalize u Conv.id
let s = eval u

let uri = s 3 5. [1;2] "bla"

let () = Format.printf "%a\n%!" Uri.pp_hum uri
