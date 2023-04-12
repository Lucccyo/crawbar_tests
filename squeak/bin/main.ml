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



(** Source file positions: [(filename, line, column)] *)


(** Environment variable update operators *)
type env_update_op = Eq       (** [=] *)
                   | PlusEq   (** [+=] *)
                   | EqPlus   (** [=+] *)
                   | ColonEq  (** [:=] *)
                   | EqColon  (** [=:] *)
                   | EqPlusEq (** [=+=] *)


(** [with_pos] type, used for all units, embedding the element [pelem] ans
      its position [pos] *)
 *)

(* type value = OpamParserTypes.FullPos.value *)


(* type file_name = string

type pos = file_name * int * int

type 'a with_pos = {
  pelem : 'a;
  pos : pos
}

type value_kind = OpamParserTypes.FullPos.value_kind
type value = OpamParserTypes.FullPos.value_kind OpamParserTypes.FullPos.with_pos *)

let value_kind =
  let open Crowbar in
  map[range 11] (fun n ->
    match n with
    | 0 -> OpamParserTypes.FullPos.Bool (Random.bool ())
    (* | 1 -> OpamParserTypes.FullPos.Int (pos, Random.int 100) *)
    | _ -> assert false)

let () =
  let pos : OpamParserTypes.FullPos.pos = {filename = ""; start = 0, 0; stop = 0,0} in
  Crowbar.(add_test ~name:"identity function" [value_kind] (fun value_kind ->
    let value : OpamParserTypes.FullPos.value= {pelem = value_kind; pos} in
    OpamPrinter.FullPos.format_value Format.std_formatter value
  ))
