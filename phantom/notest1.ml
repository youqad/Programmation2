
open File

let () =
  let fd = open_wo "foo" in
    ignore (read fd)
