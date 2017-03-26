(* Modifier ce fichier pour séparer les fichiers par
 * typage: pas de write sur un fichier ouvert en read-only,
 * etc.
 * Faire en sorte que l'utilisateur du module n'ait pas à
 * utiliser de coercions explicites. *)

type 'a handle = Unix.file_descr

type rw = unit
type ro = unit
type wo = unit

val open_rw : string -> rw handle
val open_ro : string -> ro handle
val open_wo : string -> wo handle

val read : unit handle -> string
val write : unit handle -> string -> unit

val close : 'a handle -> unit
