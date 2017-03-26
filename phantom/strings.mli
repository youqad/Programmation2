type +'a str
type clean
type dirty

val length : 'a str -> int
val read : unit -> dirty str
val write : clean str -> unit
val sanitize : dirty str -> clean str
