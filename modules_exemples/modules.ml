(** Ce fichier contient le code des premiers slides du cours
  * sur le système de modules d'OCaml, avant les démos. *)

(** {1 Modules} *)

module List = struct
  type 'a list = Nil | Cons of 'a * 'a list
  let rec mem x = function
    | Nil -> false
    | Cons (a,l) -> a = x || mem x l
end

let l : int List.list = List.Cons (1, List.Nil)

(** {2 Les constructions open et include} *)

module SetList = struct
  type 'a set = 'a List.list
  let empty = List.Nil
  let add x l =
    if List.mem x l then l else List.Cons (x,l)
end

module SetList_open = struct
  open List
  type 'a set = 'a list
  let empty = Nil
  let add x l =
    if mem x l then l else Cons (x,l)
end

module SetList_incl = struct
  include List
  type 'a set = 'a list
  let empty = Nil
  let add x l =
    if mem x l then l else Cons (x,l)
end

(** Ceci ne fonctionnerait pas avec SetList_open. *)
let () = ignore SetList_incl.Nil

(* List est déja défini dans ce module (Modules) et je ne peux pas y faire
 * une seconde définition, je nomme donc List_ext. *)
module List_ext = struct
  include List
  let rec length = function
    | Nil -> 0
    | Cons (_,l) -> 1 + length l
end

(** {1 Signatures} *)

module type LIST = sig
  type 'a list = Nil | Cons of 'a * 'a list
  val mem : 'a -> 'a list -> bool
end

module L : LIST = List

(** Utilisation de [open Module] dans une signature. *)
module type SETLIST = sig
  open List
  type 'a set = 'a list
  val empty : 'a set
  val add : 'a -> 'a set -> 'a set
end

module S : SETLIST = SetList

(** Utilisation de [include SIGNATURE]. *)
module type SETLIST2 = sig
  include LIST
  type 'a set = 'a list
  val empty : 'a set
  val add : 'a -> 'a set -> 'a set
end

(** Ceci ne marcherait pas avec SetList ou SetList_open. *)
module S2 : SETLIST2 = SetList_incl
