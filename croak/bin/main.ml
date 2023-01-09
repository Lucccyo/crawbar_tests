open Zed_string
open Uchar

let c_to_utf8 c = of_utf8 (Char.(escaped c))

let c_to_uchar_to_utf8 c = 
  let utf8, remaining = of_uChars [of_char c] in
  assert (remaining = []);
  utf8

let () =
  Crowbar.(add_test ~name:"testing utf8 conversion" [char] (fun c -> check_eq (c_to_utf8 c) (c_to_uchar_to_utf8 c)))
