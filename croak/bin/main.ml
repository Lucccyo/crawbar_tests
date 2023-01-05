let f l _a = 
  List.rev l

let () =
  Crowbar.(add_test ~name:"identity A" [list int ; int] (fun l a -> check_eq l (f l a)));
  Crowbar.(add_test ~name:"identity B" [list int] (fun l -> check_eq l []))