
open File

(** Test: conversions *)
let rw_to_ro (fd : rw handle) = (fd :> ro handle)
let rw_to_wo (fd : rw handle) = (fd :> wo handle)

(** Test: écriture *)
let () =
  let fd = open_wo "foo" in
    write fd "toto titi tutu\n" ;
    close fd

(** Test: lecture + écriture *)
let () =
  let fd = open_rw "foo" in
    write fd (String.uppercase (read fd)) ;
    close fd
