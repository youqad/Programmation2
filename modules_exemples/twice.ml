(** Dans une structure...
  * Interdit:
  *   double def d'un nom de module, de signature, de type.
  * Permis:
  *   double exception, valeur. *)

(** Dans une sig
  * Interdit: double type
  * Permis: double valeur *)

module type F = sig
  val t : int
  val t : int
end
