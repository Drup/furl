(** The world bank api *)

(** The goal of this file is to implement the world bank's REST API.
    It's documentation is available here: http://data.worldbank.org/node/11
*)

(** {2 Common parameters} *)

(** Common parameters.
    These parameters are used by all calls.
*)
(* let general_params () = Furl.( *)
(*     ("date", String) ** *)
(*       ("format", *)
