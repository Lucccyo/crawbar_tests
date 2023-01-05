type os = 
  | MacOS
  | Linux
  | Win32

let f os = 
  match os with
  | MacOS -> Unix.sleep 2
  | Linux -> Unix.sleep 0
  | Win32 -> Unix.sleep 0

let os = 
  let open Crowbar in
  map[range 3] (fun n ->
    match n with
    | 0 -> MacOS
    | 1 -> Linux
    | 2 -> Win32
    | _ -> assert false)

let () =
  Crowbar.(add_test ~name:"identity function" [os] (fun os -> f os))
