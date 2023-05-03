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
                           with_pos (Int 1),
                           with_pos (Int 2))),
          with_pos (Int 3)))

let poire = with_pos
  (Logop (with_pos `And,
          with_pos (Int 1),
          with_pos (Logop (with_pos `Or,
                           with_pos (Int 2),
                           with_pos (Int 3)))))

let rec add_group ?(p = -1) value context =
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
