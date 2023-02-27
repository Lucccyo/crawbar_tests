let index_end_word str pos =
  let rope = (Zed_rope.of_string (Zed_string.of_utf8 str)) in
  match Zed_edit.default_match_word rope pos with
  | None -> Format.printf "pos = %d\tNone@." pos
  | Some n -> Format.printf "pos = %d\tSome %d@." pos n

let () =
  let str = "meow" in
  for i = 0 to String.length str do
    index_end_word str i
  done