type value = OpamParserTypes.FullPos.value_kind OpamParserTypes.FullPos.with_pos

let pp_value ppf ({pelem = value_kind; _} : value) =
  match value_kind with
  | Int n    -> Format.fprintf ppf "Int %d" n
  | Bool b   -> Format.fprintf ppf "Bool %b" b
  | String s -> Format.fprintf ppf "Bool %s" s
  | _ -> assert false

let gen_value =
  let open Crowbar in
  let open OpamParserTypes.FullPos in
  (* fix (fun gen_value -> *)
    choose [
    map [int]   (fun i -> Int i);
    map [bool]  (fun b -> Bool b);
    map [bytes] (fun s -> String s)]
  (* ) *)


let () =
  let open OpamPrinter.FullPos in
  let open OpamParserTypes.FullPos in
  let pos = { filename = ""; start = 0,0; stop = 0,0 } in
  Crowbar.(add_test ~name:"identity function" [gen_value] (fun value_kind ->
    let v = { pelem = value_kind; pos } in
    let printed = value v in
    check_eq ~pp:pp_value ~eq:value_equals v (OpamParser.FullPos.value_from_string printed "")
  ))
