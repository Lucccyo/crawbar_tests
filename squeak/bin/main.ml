type value = OpamParserTypes.FullPos.value_kind OpamParserTypes.FullPos.with_pos
(* type relop = OpamParserTypes.FullPos.relop_kind OpamParserTypes.FullPos.with_pos *)

let pp_relop ppf relop =
  match relop with
  | `Eq  -> Format.fprintf ppf "="
  | `Neq -> Format.fprintf ppf "!="
  | `Geq -> Format.fprintf ppf ">="
  | `Gt  -> Format.fprintf ppf ">"
  | `Leq -> Format.fprintf ppf "<="
  | `Lt  -> Format.fprintf ppf "<"
  | _ -> assert false


let rec pp_value ppf ({pelem = value_kind; _} : value) =
  match value_kind with
  | Int n    -> Format.fprintf ppf "Int [%d]" n
  | Bool b   -> Format.fprintf ppf "Bool [%b]" b
  | String s -> Format.fprintf ppf "String [%S]" s
  | Relop (r, v1, v2) ->
    Format.fprintf ppf "Relop [%a %a %a]" pp_value v1 pp_relop r.pelem pp_value v2
  | List {pelem = l; _} -> Format.fprintf ppf "List [\t%a]" pp_values l
  | _ -> assert false
and pp_values fmt = function
| [] -> ()
| [v] -> pp_value fmt v
| v :: tl ->
  pp_value fmt v;
  Format.fprintf fmt " ; ";
  pp_values fmt tl


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

let gen_logop : OpamParserTypes.FullPos.logop Crowbar.gen =
  let open Crowbar in
  map[range 2] (fun n ->
    match n with
    | 0 -> with_pos `And
    | 1 -> with_pos `Or
    | _ -> assert false)


let gen_pfxop : OpamParserTypes.FullPos.pfxop Crowbar.gen =
  let open Crowbar in
  map[range 2] (fun n ->
    match n with
    | 0 -> with_pos `Not
    | 1 -> with_pos `Defined
    | _ -> assert false)

let gen_value =
  let open Crowbar in
  let open OpamParserTypes.FullPos in
  fix (fun gen_v ->
    choose [
    (* map [int]   (fun i -> with_pos (Int i)); *)
    map [bool]  (fun b -> with_pos (Bool b));
    (* map [bytes] (fun _ -> with_pos (String "toto")); *)
    map [gen_relop; gen_v; gen_v] (fun r v1 v2 -> with_pos (Relop (r, v1, v2)));
    (* map [gen_relop; gen_v] (fun r v -> with_pos (Prefix_relop (r, v))); *)
    (* map [gen_logop; gen_v; gen_v] (fun l v1 v2 -> with_pos (Logop (l, v1, v2))); *)
    (* map [gen_pfxop; gen_v] (fun p v -> with_pos (Pfxop (p, v))) *)
    ] )

let () =
  let open OpamParserTypes.FullPos in
  let v = with_pos (Int 4) in
  (* let r = with_pos
    (Relop (with_pos `Lt,
            with_pos (Int 1),
            with_pos (Relop (with_pos `Lt,
                             with_pos (Int 1),
                             with_pos (Int 2))))) in *)
  let l = with_pos (List (with_pos [with_pos (Int 3); with_pos (Int 3)])) in
  let l = with_pos [v; v; l; v] in
  let v = with_pos (List l) in

  let printed = OpamPrinter.FullPos.value v in
  Format.print_newline();
  Format.print_newline();
  Format.printf "[value]\t\t\t%s\n" printed;
  (* let printed = "1 > (1 > 2)" in *)
  (* let printed = "1 > 1 > 2" in *)
  Format.fprintf Format.std_formatter "[pp_value]\t\t%a\n" pp_value v;
  let parsed = OpamParser.FullPos.value_from_string printed "" in
  Format.fprintf Format.std_formatter "[pp_value parseprint]\t%a\n" pp_value parsed;
  (* let parsed_printed = OpamPrinter.FullPos.value parsed in *)
  (* Format.printf "[parsed_printed]\t%s\n" printed; *)
  Format.print_newline();
  ()


  (* let open OpamPrinter.FullPos in
  Crowbar.(add_test ~name:"parse of print" [gen_value] (fun v ->
    let printed = value v in
    Format.printf ">> ";
    pp_value Format.std_formatter v;
    Format.printf "\n";
    check_eq ~pp:pp_value ~eq:value_equals v (OpamParser.FullPos.value_from_string printed "");
    ()
  )) *)
