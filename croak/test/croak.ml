let () =
  let engine = Zed_edit.create () in
  let cursor = Zed_edit.new_cursor engine in
  let ctxt = Zed_edit.context engine cursor in
  Zed_edit.insert ctxt (Zed_rope.of_string (Zed_string.of_utf8 "sdfs "));
  Zed_edit.set_mark ctxt;
  Zed_edit.insert ctxt (Zed_rope.of_string (Zed_string.of_utf8 "\u{c7}sbcouh"));
  Zed_edit.goto_mark ctxt;
  Zed_edit.delete_next_word ctxt;
  Format.printf "[%s]@." (Zed_edit.text engine |> Zed_rope.to_string |> Zed_string.to_utf8)

(*
  c82b02865a86  fix default match word
  da7438bb909c  reproduce error with printf
  0b35ef6982059fa  solve little problems around the big one *)


(* let default_match_word =
  let rec loop_start segmenter zip =
    match Zed_rope.Zip_raw.next zip with
    | exception Zed_rope.Out_of_bounds -> None
    | ch, zip ->
      match Uuseg.add segmenter (`Uchar ch) with
      | `Await          -> loop_start segmenter zip
      | `Uchar _ | `End -> None
      | `Boundary       -> loop_word segmenter zip ~pos:0 `Await
  and loop_word segmenter zip v ~pos =
    match Uuseg.add segmenter v with
    | `Boundary | `End -> Some pos
    | `Uchar _         -> loop_word segmenter zip `Await ~pos:(pos + 1)
    | `Await           ->
      match Zed_rope.Zip_raw.next zip with
      | exception Zed_rope.Out_of_bounds -> Some (pos + 1)
      | ch, zip -> loop_word segmenter zip (`Uchar ch) ~pos
  in
  fun rope idx ->
    let zip = Zed_rope.Zip_raw.make_f rope idx in
    match loop_start (Uuseg.create `Word) zip with
    | Some pos -> Some (idx + pos) | None -> None *)