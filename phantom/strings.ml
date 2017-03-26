type 'a str = string
type clean = unit
type dirty = unit

let length s = String.length s
let read () = read_line ()
let write s = Printf.printf "%s\n" s
let sanitize s = Filename.quote s
