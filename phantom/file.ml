
type 'a handle = Unix.file_descr

type rw = unit
type ro = unit
type wo = unit

(* Le code ci-dessous ne bouge pas. *)

let open_ro path =
  Unix.openfile path [Unix.O_RDONLY] 0o644

let open_wo path =
  Unix.openfile path [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_APPEND] 0o644

let open_rw path =
  Unix.openfile path [Unix.O_RDWR;Unix.O_CREAT;Unix.O_APPEND] 0o644

let close fd = Unix.close fd

let read fd =
  let len = 1024 in
  let s = String.create len in
  let n = Unix.read fd s 0 len in
    if n < 0 then failwith "Error: read" ;
    String.sub s 0 n

let write fd s =
  let len = String.length s in
  let n = Unix.write fd s 0 len in
    if n <> len then failwith "Error: write"
