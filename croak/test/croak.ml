let () =
  let engine = Zed_edit.create () in
  let cursor = Zed_edit.new_cursor engine in
  let ctxt = Zed_edit.context engine cursor in
  Zed_edit.insert ctxt (Zed_rope.of_string (Zed_string.of_utf8 "helio"));
  Zed_edit.set_mark ctxt;
  Zed_edit.insert ctxt (Zed_rope.of_string (Zed_string.of_utf8 " world"));
  Zed_edit.goto_mark ctxt;
  Zed_edit.kill_next_word ctxt