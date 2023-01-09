(* open Zed_string *)
open Zed_utf8
(* open Uchar *)

(* let c_to_utf8 c = of_utf8 (Char.(escaped c)) *)

(* let c_to_uchar_to_utf8 c = 
  let utf8, remaining = of_uChars [of_char c] in
  assert (remaining = []);
  utf8 *)

(* type zutf8 = Zed_utf8.t *)

let contain_first_char str = 
  match check str with 
  | Correct length ->
    if length > 0 then 
      let first = sub str 0 1 in
      assert (contains str first)
    else ()
  | Message _m -> () 

let () =
  Crowbar.(add_test ~name:"testing utf8 conversion" [bytes] (fun str -> contain_first_char str))
