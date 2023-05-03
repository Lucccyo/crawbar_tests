open OpamParserTypes.FullPos

(* type value = OpamParserTypes.FullPos.value_kind OpamParserTypes.FullPos.with_pos *)

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


let pomme = with_pos
  (Logop (with_pos `Or,
          with_pos (Logop (with_pos `Or,
                           with_pos (Int 1),
                           with_pos (Int 2)
                          )),
          with_pos (Int 3)
         )
  )

let patate = with_pos
  (Logop (with_pos `Or,
          with_pos (Int 1),
          with_pos (Logop (with_pos `Or,
                           with_pos (Int 2),
                           with_pos (Int 3)
                          ))
         )
  )


let carotte = with_pos
  (Logop (with_pos `Or,
          with_pos (Logop (with_pos `And,
                            with_pos (Int 1),
                            with_pos (Int 2)
                          )),
          with_pos (Int 3)
          )
  )

let tomate = with_pos
  (Logop (with_pos `Or,
          with_pos (Int 1),
          with_pos (Logop (with_pos `And,
                            with_pos (Int 2),
                            with_pos (Int 3)
                          ))
          )
  )

let melon = with_pos
  (Logop (with_pos `And,
          with_pos (Logop (with_pos `And,
                            with_pos (Int 1),
                            with_pos (Int 2)
                          )),
          with_pos (Int 3)
          )
  )

let kiwi = with_pos
  (Logop (with_pos `And,
          with_pos (Int 1),
          with_pos (Logop (with_pos `And,
                            with_pos (Int 2),
                            with_pos (Int 3)
                          ))
          )
  )

let radis = with_pos
  (Logop (with_pos `And,
          with_pos (Logop (with_pos `Or,
                            with_pos (Int 1),
                            with_pos (Int 2)
                          )),
          with_pos (Int 3)
          )
  )

let poire = with_pos
  (Logop (with_pos `And,
          with_pos (Int 1),
          with_pos (Logop (with_pos `Or,
                            with_pos (Int 2),
                            with_pos (Int 3)
                          ))
          )
  )

let rec add_group ?(p = -1) value context =
  (* with_pos (Group (with_pos [with_pos value_kind])) *)
  match value.pelem with
  | Logop (op, lvk, rvk) ->
    if context = None then
      with_pos (Logop (op,
          add_group lvk (Some (op.pelem)) ~p:0,
          add_group rvk (Some (op.pelem)) ~p:1))
    else
      if (p = 1 && Option.get context = op.pelem) ||
         (Option.get context = `And && op.pelem = `Or) then
      with_pos (Group (with_pos [value]))
    else value
  | Int n -> with_pos (Int n)
  | _ -> assert false


let add_group value = add_group value None

let pp_value_and_grouped v name =
  Format.print_newline();
  let printed = OpamPrinter.FullPos.value v in
  Format.printf "[%s]\t%s\n" name printed;
  let printed = OpamPrinter.FullPos.value (add_group v) in
  Format.printf "[G-%s]\t%s\n" name printed;
  Format.print_newline()


let () =
  (* add_group pomme;
  add_group patate;
  add_group carotte;
  add_group tomate;
  add_group melon;
  add_group kiwi;
  add_group radis;
  add_group poire; *)


  pp_value_and_grouped pomme   "pom";
  pp_value_and_grouped patate  "pat";
  pp_value_and_grouped carotte "car";
  pp_value_and_grouped tomate  "tom";
  pp_value_and_grouped melon   "mel";
  pp_value_and_grouped kiwi    "kiw";
  pp_value_and_grouped radis   "rad";
  pp_value_and_grouped poire   "poi";
  ()
