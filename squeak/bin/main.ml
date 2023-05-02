type value = OpamParserTypes.FullPos.value_kind OpamParserTypes.FullPos.with_pos

let pp_relop ppf relop =
  match relop with
  | `Eq  -> Format.fprintf ppf "="
  | `Neq -> Format.fprintf ppf "!="
  | `Geq -> Format.fprintf ppf ">="
  | `Gt  -> Format.fprintf ppf ">"
  | `Leq -> Format.fprintf ppf "<="
  | `Lt  -> Format.fprintf ppf "<"
  | _ -> assert false

let pp_logop ppf logop =
  match logop with
  | `And -> Format.fprintf ppf "&"
  | `Or  -> Format.fprintf ppf "|"
  | _ -> assert false

let pp_pfxop ppf pfxop =
  match pfxop with
  | `Not -> Format.fprintf ppf "!"
  | `Defined -> Format.fprintf ppf "?"
  | _ -> assert false

let pp_env_update_op ppf env_update_op =
  match env_update_op with
  | `Eq  -> Format.fprintf ppf "="
  | `PlusEq -> Format.fprintf ppf "+="
  | `EqPlus -> Format.fprintf ppf "=+"
  | `EqPlusEq -> Format.fprintf ppf "=+="
  | `ColonEq  -> Format.fprintf ppf ":="
  | `EqColon  -> Format.fprintf ppf "=:S"
  | _ -> assert false

let rec pp_value ppf ({pelem = value_kind; _} : value) =
  match value_kind with
  | Int n    -> Format.fprintf ppf "Int [%d]" n
  | Bool _b   -> Format.fprintf ppf "a"
  | String s -> Format.fprintf ppf "String [%S]" s
  | Relop (r, v1, v2) ->
    Format.fprintf ppf "Relop [%a %a %a]" pp_value v1 pp_relop r.pelem pp_value v2
  | Logop (l, v1, v2) ->
    Format.fprintf ppf "(%a %a %a)" pp_value v1 pp_logop l.pelem pp_value v2
  | Pfxop (p, v) ->
    Format.fprintf ppf "Pfxop [%a %a]" pp_pfxop p.pelem pp_value v
  | List {pelem = l; _} -> Format.fprintf ppf "List [\t%a]" pp_values l
  | Group {pelem = l; _} -> Format.fprintf ppf "Group [\t%a]" pp_values l
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

let () =
  let open OpamParserTypes.FullPos in
  let pomme = with_pos
    (Logop (with_pos `Or,
            with_pos (Logop (with_pos `Or,
                             with_pos (Int 1),
                             with_pos (Int 2)
                            )),
            with_pos (Int 3)
           )
    ) in

  let patate = with_pos
    (Logop (with_pos `Or,
            with_pos (Int 1),
            with_pos (Logop (with_pos `Or,
                             with_pos (Int 2),
                             with_pos (Int 3)
                            ))
           )
    ) in


  let carotte = with_pos
    (Logop (with_pos `Or,
            with_pos (Logop (with_pos `And,
                              with_pos (Int 1),
                              with_pos (Int 2)
                            )),
            with_pos (Int 3)
            )
    ) in

  let tomate = with_pos
    (Logop (with_pos `Or,
            with_pos (Int 1),
            with_pos (Logop (with_pos `And,
                              with_pos (Int 2),
                              with_pos (Int 3)
                            ))
            )
    ) in

  let melon = with_pos
    (Logop (with_pos `And,
            with_pos (Logop (with_pos `And,
                              with_pos (Int 1),
                              with_pos (Int 2)
                            )),
            with_pos (Int 3)
            )
    ) in

  let kiwi = with_pos
    (Logop (with_pos `And,
            with_pos (Int 1),
            with_pos (Logop (with_pos `And,
                              with_pos (Int 2),
                              with_pos (Int 3)
                            ))
            )
    ) in

  let radis = with_pos
    (Logop (with_pos `And,
            with_pos (Logop (with_pos `Or,
                              with_pos (Int 1),
                              with_pos (Int 2)
                            )),
            with_pos (Int 3)
            )
    ) in

  let poire = with_pos
    (Logop (with_pos `And,
            with_pos (Int 1),
            with_pos (Logop (with_pos `Or,
                              with_pos (Int 2),
                              with_pos (Int 3)
                            ))
            )
    ) in

  let pp_value v =
    let printed = OpamPrinter.FullPos.value patate in
    Format.print_newline();
    Format.printf "[value]\t\t\t%s\n" printed;
    Format.print_newline() in
  pp_value pomme;
  pp_value patate;
  pp_value carotte;
  pp_value tomate;
  pp_value melon;
  pp_value kiwi;
  pp_value radis;
  pp_value poire;
  ()
