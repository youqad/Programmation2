
open File

let () =
  let fd = open_ro "foo" in
    write fd "" ;
    close fd
