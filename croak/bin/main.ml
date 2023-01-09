open Zed_string

let uint_to_utf8 u = of_utf8 (Char.(escaped (chr u)))

let () =
(* Format.printf "%b\n" ((of_utf8 "\001") = (copy (of_utf8 "\001")));
Format.printf "%b\n" ((of_utf8 "\001") == (copy (of_utf8 "\001"))); *)
(* Format.printf "\n%c\n" (Char.chr 128); *)
  Crowbar.(add_test ~name:"testing utf8 conversion" [int] (fun u -> check_eq (uint_to_utf8 u) (copy (uint_to_utf8 u))))
