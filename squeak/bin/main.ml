open OpamParserTypes.FullPos

let with_pos v = { pelem = v; pos = { filename = ""; start = 0,0; stop = 0,0 } }

let pomme = with_pos
  (Logop (with_pos `Or,
          with_pos (Logop (with_pos `Or,
                           with_pos (Int 1),
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
                           with_pos (Int 2))),
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

let decide context p pelem =
  (p && context = pelem) || (context = `And && pelem = `Or)

let rec add_group value context p =
  match value.pelem with
  | Logop (op, lvk, rvk) ->
      let v = logop op lvk rvk in
      if decide context p op.pelem
        then with_pos (Group (with_pos [v]))
        else v
  | Int _ -> value
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
