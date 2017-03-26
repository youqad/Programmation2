module type COLLECTION = sig
  type elt
  type t
  val iter : (elt -> unit ) -> t -> unit
end

module type PERSITER = sig
  type elt
  type t
  type collection
  val init : collection -> t
  val next : t -> (elt * t) option
end

module type ELT = sig type t end

module ListCol (E : ELT ) : COLLECTION
  with type elt =E.t and type t = E.t list = struct
  type elt = E.t
  type t = elt list
  let rec iter f = function
    | [] -> ()
    | a :: l -> f a ; iter f l
end

module ListPersiter (E : ELT ) : PERSITER with type elt = E.t = struct
  type elt = E.t
   type t = elt list
  type collection = elt list
  let init l = l
  let next = function
    | [] -> None
    | a :: l -> Some (a , l)
end
 
module type COL_OF_PERSITER =
  functor (P : PERSITER) -> (COLLECTION with type elt = P.elt and type
                              t=P.collection)

module ColOfPersister : COL_OF_PERSITER =
  functor (P: PERSITER) ->
  struct
    type elt = P.elt
    type t = P.collection
    let iter f c =
      let rec iter_aux t = match (P.next t) with
      | None -> ()
      | Some (el, t) -> (f el; iter_aux t)
      in iter_aux (P.init c)
  end

module type PERSITER_OF_COL =
  functor (C : COLLECTION) -> (PERSITER with type elt =C.elt and type
                                 collection=C.t)

(* module PersiterOfCol : PERSITER_OF_COL = *)
(*   functor (C : COLLECTION) -> *)
(*   struct *)
(*     type elt = C.elt *)
(*     type collection = C.t *)
(*     type t = C.t *)
(*     (\* val init : collection -> t  *\) *)
(*     let next (t: t) = *)
(*       let touched = ref false and result = ref None in *)
(*       ( *)
(*         C.iter (fun el -> if not !touched then result := Some (el, t)) t; *)
(*         !result *)
(*       ) *)
(*      let init c = c *)
(*   end *)

(* module PersiterOfCol : PERSITER_OF_COL = *)
(*   functor (C : COLLECTION) -> *)
(*   struct *)
(*     type elt = C.elt *)
(*     type collection = C.t *)
(*     type t = C.t * int *)
(*     let next (t: t) = let (c, n) = t in *)
(*       let num = ref 0 and result = ref None in *)
(*       ( *)
(*         C.iter (fun el -> if !num=n then result := Some (el, (c, n+1)) *)
(*                                                         else incr num) c; *)
(*         !result *)
(*       ) *)
(*      let init c = (c, 0) *)
(* end *)

module PersiterOfCol : PERSITER_OF_COL =
  functor (C : COLLECTION) ->
  struct
    type elt = C.elt
    type collection = C.t
    type t = C.elt list
    let next (t: t) = match t with
      | [] -> None
      | hd::tl -> Some (hd, tl)
    let init (c:collection) = let list = ref [] in
      (
      C.iter (fun el -> list := el::!list) c;
      !list
      )
  end


(***** Q2 *****)


type 'a cont = unit -> 'a
module type CPSCOLLECTION = sig
  type elt
  type t
  val iter : ( elt -> 'a cont -> 'a cont ) -> t -> 'a cont -> 'a cont
end

module CpsListCol (E: ELT ) : CPSCOLLECTION with type elt = E.t and
type t = E.t list = struct
  type elt = E.t
  type t = E.t list
  let iter f l k =
    let rec iter_aux l k = match l with
      | [] -> k
      | a :: l -> f a ( iter_aux l k)
    in iter_aux l k
end

module type PERSITER_OF_CPSCOL =
  functor (C : CPSCOLLECTION ) -> ( PERSITER with type elt =C. elt and
                                  type collection =C .t)
module type CPSCOL_OF_PERSITER =
  functor (P : PERSITER ) -> ( COLLECTION with type elt =P. elt and type
                               t= P. collection )

(* module PersiterOfCPSCol : PERSITER_OF_CPSCOL = *)
(*   functor (C : CPSCOLLECTION) -> *)
(*   struct *)
(*     type elt = C.elt *)
(*     type collection = C.t *)
(*     type t = {k : (C.elt * t) option cont} *)
(*     let next it = it.k() *)
(*   end *)


