(* let f str =
  match Zed_string.of_utf8 str with
  | _ -> ()
  | exception Zed_string.Invalid _ -> ()
  | exception Zed_utf8.Invalid _ -> () *)

let f str =
  if str <> "toto" then () else failwith "non."

let () =
Crowbar.(add_test ~name:"testing utf8 conversion" [bytes] (fun str -> f str))



(* Format.printf "%b\n" ((of_utf8 "\001") = (copy (of_utf8 "\001")));
Format.printf "%b\n" ((of_utf8 "\001") == (copy (of_utf8 "\001"))); *)
(* Format.printf "\n%c\n" (Char.chr 128); *)
(* let index_end_word str pos =
  let rope = Zed_rope.of_string (Zed_string.of_utf8 str) in
  match Zed_edit.default_match_word rope pos with
  | None -> Format.printf "pos = %d\tNone@." pos
  | Some n -> Format.printf "pos = %d\tSome %d@." pos n

let f str =
  Format.printf "\n=== [%s] ===@." str;
  for i = 0 to String.length str - 1 do
    index_end_word str i
  done

let () =
  f "meow meow meow";
  f "meow";
  f "m";
  f " ";
  f "" *)
