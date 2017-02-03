(** Quelques exemples à compiler avec ocamlc -dlambda dlambda.ml *)

(** Module à deux champs visibles. *)

module Compteur = struct
  let cur = ref 0
  let next () = incr cur ; !cur
end

(** Cachons un champ. *)

module type COMPTEUR = sig
  val next : unit -> int
end

module C : COMPTEUR = Compteur

module C2 : COMPTEUR = struct
  let cur = ref 0
  let next () = incr cur ; !cur
end

(** Pour comparer:
  * définition d'un enregistrement et accès à un de ses champs. *)

type r = { p : int ; q : int }
let f x = { p = x ; q = x }
let c = f 1
let d = c.p

(** Divers types de champs visibles. *)

module L = struct
  type t = int list
  exception Foo
  let add l = 1 :: l
  module type T = Set.OrderedType
  module M = List
end
