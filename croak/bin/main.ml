(* let valid_zed_string uchar_l =
  let zed_str, _ = Zed_string.of_uChars uchar_l in
  zed_str

let uchar_list =
  let open Crowbar in
  list uchar *)
(*
let uchar_list =
  let open Crowbar in
  list uchar *)

let is_valid_zchar s =
  match Zed_char.of_utf8 s with
  | _ -> true
  | exception Failure _ -> false
  | exception Zed_utf8.Invalid _ -> false

let is_valid_zrope n zchr =
  match Zed_rope.make n zchr with
  | _ -> true
  | exception Zed_rope.Out_of_bounds -> false

let () =
  (* let f s _ = if s = "Hello" then failwith "ba" else () in *)
  Crowbar.(add_test ~name:"width function" [int; int; bytes] (fun n m s ->
    guard(is_valid_zchar s);
    let zchr = Zed_char.of_utf8 s in
    guard(is_valid_zrope n zchr);
    let zrope = Zed_rope.make n zchr in

    (* let zed_str = valid_zed_string uchar_l in *)
    (* Crowb r.check_eq zed_str (Zed_string.implode (Zed_string.explode zed_str)) *)
    (* match Zed_rope.of_string zed_str with
    | _ -> ()
    | exception Zed_string.Invalid _ -> ()
    (* | exception Zed_utf8.Out_of_bounds -> () *)
    (* | exception Invalid_argument _ -> () *)
    | exception Zed_utf8.Invalid _ -> () *)
    match Zed_rope.sub zrope n m with
    | _ -> ()
    (* | exception Invalid_argument _ -> () *)
    (* | exception Failure _ -> () *)
    | exception Zed_string.Invalid _ -> ()
    | exception Zed_rope.Out_of_bounds -> ()
    (* | exception Invalid_argument _ -> () *)
    | exception Zed_utf8.Invalid _ -> ()
  ))
