let infinity str =
  if str <> "toto" then () else failwith "the input must be different to 'toto'"

let speedy str =
  if str = "toto" then () else failwith "the input must be 'toto'"

let () =
Crowbar.(add_test ~name:"fail if the input is different to 'toto'" [bytes] (fun str -> speedy str));
Crowbar.(add_test ~name:"fail if the input is 'toto'" [bytes] (fun str -> infinity str))
