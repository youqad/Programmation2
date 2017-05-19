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
  functor (P : PERSITER ) -> ( CPSCOLLECTION with type elt =P. elt and type
                               t= P. collection )

(* module PersiterOfCPSCol : PERSITER_OF_CPSCOL = *)
(*   functor (C : CPSCOLLECTION) -> *)
(*   struct *)
(*     type elt = C.elt *)
(*     type collection = C.t *)
(*     type t = {k : (C.elt * t) option cont} *)
(*     let next it = it.k() *)
(*   end *)


module CPSColOfPersiter : CPSCOL_OF_PERSITER =
  functor (P: PERSITER) ->
  struct
    type elt = P.elt
    type t = P.collection
    let iter f c k =
      let rec iter_aux t k' = match (P.next t) with
      | None -> k'
      | Some (el, t) -> iter_aux t (f el k')
      in iter_aux (P.init c) k
  end


module type COL_OF_CPSCOL = functor (C:CPSCOLLECTION) ->
  (COLLECTION with type elt = C.elt and type t = C.t)

module CPSColOfCol : COL_OF_CPSCOL =
  functor (C: CPSCOLLECTION) -> struct
    type elt = C.elt
    type t = C.t
    let iter (f: elt -> unit) c =
      let f2 x k () = (f x; k()) in
      let k0 () = () in
      (C.iter f2 c k0) ()
  end

module PersiterOfCPSCol : PERSITER_OF_CPSCOL =
  functor (C: CPSCOLLECTION ) -> struct
    type elt = C.elt
    type collection = C.t
    type t = {cont : (C.elt * t) option cont }
    let next it = it.cont()
    let init c =
      {
        cont=
          C.iter
            (fun x k () -> Some(x, {cont=k}))
            c (
            fun () -> None)
      }
  end


(* EX 2 *)

module type ELT = sig type t end

type 'a btree =
  | Leaf of 'a
  | Node of 'a btree * 'a btree

module type BTREECOL = functor ( E: ELT ) ->
  (COLLECTION with type elt = E.t and type t = E.t btree)


module BTreeCol : BTREECOL =
  functor (E: ELT) -> struct
    type elt = E.t
    type t = E.t btree
    let rec iter f t = match t with
      | Leaf (a) -> f a
      | Node (a, b) -> (iter f a; iter f b)
   end

let bt = Node(Node(Leaf (1) , Leaf (2)), Leaf (3))

module M = BTreeCol(struct type t = int end)

let _ = M.iter print_int bt

module type BTREECPSCOL = functor (E : ELT ) ->
  ( CPSCOLLECTION with type elt = E.t and type t = E.t btree )

module BTreeCPSCol : BTREECPSCOL =
  functor (E: ELT) -> struct
    type elt = E.t
    type t = E.t btree
    let rec iter f t k = match t with
      | Leaf (a) -> f a k
      | Node (a, b) -> iter f a (iter f b k)
    end

module M' = BTreeCPSCol(struct type t = int end)

let _ = (M'.iter (fun x k () -> print_int x; k()) bt (fun () -> ())) ()


module BTreePersiter (E : ELT ) = struct
  type elt = E.t
  type collection = E.t btree
  type t = E.t btree list
  let next t = match t with
    | [] -> None
    | h::t -> Some (h, t)
  let init c =  let rec fold = function
      | Leaf (a) -> [a]
      | Node (a, b) -> (fold a)@(fold b) in
    fold c
end
