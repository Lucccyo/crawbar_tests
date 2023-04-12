

(* type value =
  | Bool of pos * bool
  | Int of pos * int
  | String of pos * string

  | Relop of pos * relop * value * value
  | Prefix_relop of pos * relop * value
  | Logop of pos * logop * value * value
  | Pfxop of pos * pfxop * value

  | Ident of pos * string
  | List of pos * value list
  | Group of pos * value list
  | Option of pos * value * value list
  | Env_binding of pos * value * env_update_op * value
*)

(* let relop () : OpamParserTypes.FullPos.relop =
    match Random.int 6 with
    | 0 ->
    | 1 -> `Neq
    | 2 -> `Geq
    | 3 -> `Gt
    | 4 -> `Leq
    | 5 -> `Lt
    | _ -> assert false *)

let value_kind =
  let open Crowbar in
  let open OpamParserTypes.FullPos in
  map[range 4] (fun n ->
    match n with
    | 0 -> Bool true
    | 1 -> Int 0
    | 2 -> String "string"
    | 3 -> Ident "ident"
    (* | 3 -> OpamParserTypes.FullPos.Relop (relop ()) value_kind value_kind *)
    | _ -> assert false)

let () =
  let pos : OpamParserTypes.FullPos.pos = {filename = ""; start = 0, 0; stop = 0,0} in
  Crowbar.(add_test ~name:"identity function" [value_kind] (fun value_kind ->
    let value : OpamParserTypes.FullPos.value = {pelem = value_kind; pos} in
    let printed = OpamPrinter.FullPos.value value in
    Format.printf "[%s]\n" printed;
    check_eq value (OpamParser.FullPos.value_from_string printed "")
  ))
