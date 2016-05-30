
let main_url () = Furl.host "www.thingy.com"

let company_db = [
  "foo" , [ 1 ; 2 ; 3 ] ;
  "bar" , [ 4 ] ;
  "baz" , [ 5 ; 6 ] ;
]

let list_companies () = List.map fst company_db

let get_company company =
  try Some (company, List.assoc company company_db)
  with Not_found -> None

let list_employees company = match get_company company with
  | None -> None
  | Some (_,l) -> Some l

let get_employee company employee = match list_employees company with
  | None -> None
  | Some l ->
    try Some (List.find (fun e -> employee = e) l)
    with Not_found -> None

let handle_website =
  Furl.(match_url
      ~default:(fun _uri -> failwith "Meh.")
      [
        ~$main_url/"companies"/?any                          --> list_companies () ;
        ~$main_url/"companies"/%String/?any                  --> get_company ;
        ~$main_url/"companies"/%String/"employees"/?any      --> list_employees ;
        ~$main_url/"companies"/%String/"employees"/%Int/?any --> get_employee ;
      ])
