open OpamParserTypes.FullPos

let with_pos v = { pelem = v; pos = { filename = ""; start = 0,0; stop = 0,0 } }

let pomme = with_pos
  (Logop (with_pos `Or,
          with_pos (Logop (with_pos `Or,
                           with_pos (Bool false),
                           with_pos (Int 2))),
          with_pos (Int 3)))

let patate = with_pos
  (Logop (with_pos `Or,
          with_pos (Int 1),
          with_pos (Logop (with_pos `Or,
                           with_pos (Int 2),
                           with_pos (Int 3)))))


let carotte = with_pos
  (Logop (with_pos `Or,
          with_pos (Logop (with_pos `And,
                           with_pos (Int 1),
                           with_pos (Int 2))),
          with_pos (Int 3)))

let tomate = with_pos
  (Logop (with_pos `Or,
          with_pos (Int 1),
          with_pos (Logop (with_pos `And,
                           with_pos (Int 2),
                           with_pos (Int 3)))))

let melon = with_pos
  (Logop (with_pos `And,
          with_pos (Logop (with_pos `And,
                           with_pos (Int 1),
                           with_pos (Bool true))),
          with_pos (Int 3)))

let kiwi = with_pos
  (Logop (with_pos `And,
          with_pos (Int 1),
          with_pos (Logop (with_pos `And,
                           with_pos (Int 2),
                           with_pos (Int 3)))))

let radis = with_pos
  (Logop (with_pos `And,
          with_pos (Logop (with_pos `Or,
                           pomme,
                           kiwi)),
          with_pos (Int 3)))

let poire = with_pos
  (Logop (with_pos `And,
          with_pos (Int 1),
          with_pos (Logop (with_pos `Or,
                           with_pos (Int 2),
                           with_pos (Int 3)))))

(*
let decide context p pelem =
  (p && context = pelem) || (context = `And && pelem = `Or)

let rec add_group value context p =
  match value.pelem with
  | Logop (op, lvk, rvk) ->
      let v = logop op lvk rvk in
      if decide context p op.pelem
        then with_pos (Group (with_pos [v]))
        else v
  | Int    _ -> value
  | String _ -> value
  | Bool   _ -> value
  | Ident  _ -> value
  | _ -> assert false

and logop op lvk rvk =
  let v = Logop (op,
    add_group lvk op.pelem false,
    add_group rvk op.pelem true) in
  with_pos v

let add_group value = match value.pelem with
  | Logop (op, lvk, rvk) -> logop op lvk rvk
  | Int _ -> value
  | _ -> assert false
*)

let decide context p value = match context, value with
  | Option _, _ when p -> false   (* list *)
  | (List _ | Group _), _ -> false  (* list *)
  | (Bool _ | Int _ | String _ | Ident _), _ -> assert false
  | _, (Bool _ | Int _ | String _ | Ident _) -> false
  | Logop ({ pelem = `Or ; _ }, _, _), Logop ({ pelem = `Or ; _ }, _, _) -> p
  | _, Logop ({ pelem = `Or ; _ }, _, _) -> true
  | Logop ({ pelem = `Or ; _ }, _, _), _ -> false
  | Logop ({ pelem = `And ; _ }, _, _), Logop ({ pelem = `And ; _ }, _, _) -> p
  | _, Logop ({ pelem = `And ; _ }, _, _) -> true
  | Logop ({ pelem = `And ; _ }, _, _), _ -> false
  | _, Env_binding _ -> true
  | Env_binding _, _ -> false
  | Pfxop _, _ -> false
  | _, Pfxop _ -> true
  | Option _, _ -> false
  | _, (Option _ | Relop _) -> true
  | (Relop _ | Prefix_relop _), _ -> false

let rec add_group context p value =
  let v = match value.pelem with
    | Bool _ | Int _ | String _ | Ident _ -> value
    | Relop (op, lvk, rvk) -> with_pos @@
        Relop (op, add_group value false lvk,
                   add_group value true  rvk)
    | Prefix_relop (op, lvk) -> with_pos @@
        Prefix_relop (op, add_group value false lvk)
    | Logop (op, lvk, rvk) -> with_pos @@
        Logop (op, add_group value false lvk,
                   add_group value true  rvk)
    | Pfxop (op, lvk) -> with_pos @@
        Pfxop (op, add_group value false lvk)
    | Env_binding (lvk, op, rvk) -> with_pos @@
        Env_binding (add_group value false lvk, op,
                     add_group value true  rvk)
    | List l -> with_pos @@
        List (with_pos @@ List.map (add_group value false) l.pelem)
    | Group l -> with_pos @@
        Group (with_pos @@ List.map (add_group value false) l.pelem)
    | Option (lvk, l) -> with_pos @@
        Option (add_group value false lvk,
                with_pos @@ List.map (add_group value true) l.pelem)
  in
  if decide context.pelem p v.pelem
    then with_pos (Group (with_pos [v]))
    else v

let add_group value =
  let bidon = with_pos (Group (with_pos [])) in
  add_group bidon false value (* miaou *)

let pp_value_and_grouped v name =
  let printed = OpamPrinter.FullPos.value v in
  Format.printf "\n[%s]\t%s\n" name printed;
  let printed = OpamPrinter.FullPos.value (add_group v) in
  Format.printf "[G-%s]\t%s\n\n" name printed

let () =
  pp_value_and_grouped pomme   "pom";
  pp_value_and_grouped patate  "pat";
  pp_value_and_grouped carotte "car";
  pp_value_and_grouped tomate  "tom";
  pp_value_and_grouped melon   "mel";
  pp_value_and_grouped kiwi    "kiw";
  pp_value_and_grouped radis   "rad";
  pp_value_and_grouped poire   "poi";
  ()

(*
let () =
  (* let printed = "? 4 { 5 }" in *)
  let printed = ">5" in
  let parsed = OpamParser.FullPos.value_from_string  printed printed in
  Format.printf "\n%a\n" OpamPrinter.FullPos.format_value parsed;
  ()
*)