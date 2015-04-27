# Formatted Url

This library allows to create type-safe formatted urls in the spirit of the
`format` type from the standard library. It is backed by [re][re] and [uri][uri]

[re]: https://github.com/ocaml/ocaml-re
[uri]: https://github.com/mirage/ocaml-uri

**This is work in progress, if you have any remarks or proposition, please open an issue. The API _will_ be changed until I'm happy with it. Contributions are welcome. Also, it may still be buggy.**

For now, it needs [this PR](https://github.com/ocaml/ocaml-re/pull/58). You can pin it with:
```
opam pin add re https://github.com/Drup/ocaml-re.git#pmarks
```

## Quick look

Let's imagine we want to build a REST service indexing [Camelidaes](https://species.wikimedia.org/wiki/Camelidae).

```ocaml
let camlidae = Furl.Host "www.camlidae.ml"
```

We can query a Camelidae by name:
```ocaml
let by_name () =
  Furl.(finalize @@ camlidae/"name"/%String/?Nil)
val by_name : unit -> (string -> 'a, 'a) Furl.furl
```

Notice how the type of the value describes the contained parameters.

Let's consider the type `('f,'r) Furl.furl`. `'f` is the type of a function
corresponding to the parameters of the url. `'r` is the return type, which could be anything at this point.

We can also query a list of camelidae by humps:
```ocaml
let by_humps () =
  Furl.(finalize @@ camlidae/"humps"/%Int/?Nil)
val by_humps : unit -> (int -> 'a, 'a) Furl.furl
```

This is nice, but we want to refine the list by humps to only show non extinct camelidaes:
```ocaml
let by_humps () =
  Furl.(finalize @@ camlidae/"humps"/%Int/?("extinct",Opt Bool)**Nil)
val by_humps : unit -> (int -> bool option -> 'a, 'a) Furl.furl
```

We can now build a handler answering to these endpoints:

```ocaml
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
val handle_camlidaes : Uri.t -> camlidae list
```

You can then give this handler to your favorite web server and Tada, a camelidae web API.

You can also expose the formatted urls for clients:
```ocaml
let query_by_humps = Furl.eval @@ by_humps ()
val query_by_humps : int -> bool option -> Uri.t
```

Then you can use your favorite http client to get the answer we all want:
```ocaml
fetch_http @@ query_by_hump 2 (Some false) ;;
["Bactrian camel"; "Wild camel"]
```

## Principles

furl uses GADT in a similar manner than format in the standard library, but specialized for urls and regexps. The matching against multiple uris is done with re.

urls are separated into `Furl.url` which are not yet finalized and `Furl.furl` which are finalized. This separation is done to make `Furl.url` a pure datastructure that can be serialized, moved around (and sent to the client in the context of eliom). `Furl.furl` contains an `url` and the list of converters.
