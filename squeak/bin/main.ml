(* * Relational operators
type relop = [ `Eq  (** [=] *)
             | `Neq (** [!=] *)
             | `Geq (** [>=] *)
             | `Gt  (** [>] *)
             | `Leq (** [<=] *)
             | `Lt  (** [<] *)
             ]

(** Logical operators *)
type logop = [ `And (** [&] *) | `Or (** [|] *) ]

(** Prefix operators *)
type pfxop = [ `Not (** [!] *) | `Defined (** [?] *) ]

type file_name = string

(** Source file positions: [(filename, line, column)] *)
type pos = file_name * int * int

(** Environment variable update operators *)
type env_update_op = Eq       (** [=] *)
                   | PlusEq   (** [+=] *)
                   | EqPlus   (** [=+] *)
                   | ColonEq  (** [:=] *)
                   | EqColon  (** [=:] *)
                   | EqPlusEq (** [=+=] *)


(** [with_pos] type, used for all units, embedding the element [pelem] ans
      its position [pos] *)
type 'a with_pos = {
  pelem : 'a;
  pos : pos\
} *)

type value = OpamParserTypes.FullPos.value

let value =
  let open Crowbar in
  map[range 11] (fun n ->
    match n with
    | 0 -> OpamParserTypes.FullPos.Bool (Random.bool ())
    (* | 1 -> OpamParserTypes.FullPos.Int (pos, Random.int 100) *)
    | _ -> assert false)


let () =
  Crowbar.(add_test ~name:"identity function" [value] (fun value -> OpamPrinter.FullPos.format_value Format.std_formatter value))
