
module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module type MAKESET =
  functor (E : ORDERED) ->
    sig
      type t
      type elt = E.t
      val empty : t
      val add : elt -> t -> t
      val remove : elt -> t -> t
      val member : elt -> t -> bool
      val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
    end

module Make : MAKESET =
  functor (E : ORDERED) -> struct
    type t = E.t list
    type elt = E.t
    let empty = []
    let eq e e' = E.compare e e' = 0
    let neq e e' = E.compare e e' <> 0
    let remove e s = List.filter (neq e) s
    let member e s = List.exists (eq e) s
    let add e s = if member e s then s else e :: s
    let fold f s x = List.fold_left (fun x e -> f e x) x s
  end
