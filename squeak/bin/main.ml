type value = OpamParserTypes.FullPos.value_kind OpamParserTypes.FullPos.with_pos
(* type relop = OpamParserTypes.FullPos.relop_kind OpamParserTypes.FullPos.with_pos *)

let pp_value ppf ({pelem = value_kind; _} : value) =
  match value_kind with
  | Int n    -> Format.fprintf ppf "Int %d" n
  | Bool b   -> Format.fprintf ppf "Bool %b" b
  | String s -> Format.fprintf ppf "String %S" s
  | _ -> assert false


let with_pos v : 'a OpamParserTypes.FullPos.with_pos =
  { pelem = v; pos = { filename = ""; start = 0,0; stop = 0,0 } }

let gen_relop : OpamParserTypes.FullPos.relop Crowbar.gen =
  let open Crowbar in
  map[range 6] (fun n ->
    match n with
    | 0 -> with_pos `Eq
    | 1 -> with_pos `Neq
    | 2 -> with_pos `Geq
    | 3 -> with_pos `Gt
    | 4 -> with_pos `Leq
    | 5 -> with_pos `Lt
    | _ -> assert false)

let gen_value =
  let open Crowbar in
  let open OpamParserTypes.FullPos in
  fix (fun gen_v ->
    choose [
    map [int]   (fun i -> with_pos (Int i));
    map [bool]  (fun b -> with_pos (Bool b));
    map [bytes] (fun _ -> with_pos (String "toto"));
    map [gen_relop; gen_v; gen_v] (fun r v1 v2 -> with_pos (Relop (r, v1, v2)))
    ] )


let () =
  let open OpamPrinter.FullPos in
  Crowbar.(add_test ~name:"identity function" [gen_value] (fun v ->
    let printed = value v in
    check_eq ~pp:pp_value ~eq:value_equals v (OpamParser.FullPos.value_from_string printed "")
  ))
