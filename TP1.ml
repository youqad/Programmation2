module Int = struct
  type t = int
  let compare = Pervasives.compare
end

module type Container = sig
  type t
  type elt
  val empty  : t
  val add    : elt -> t -> t
  val merge  : t -> t -> t
  val member : elt -> t -> bool
  val fold   : ('a -> elt -> 'a) -> 'a -> t -> 'a
end

module type AnyT = sig
  type t
end

module LContainer (A:AnyT) : (Container with type elt = A.t) =
struct
    type t = A.t list
    type elt = A.t
    let empty = []
    let add elt t = elt::t
    let merge t1 t2 = t1 @ t2
    let member elt t = List.mem elt t
    let fold f a t = List.fold_left f a t
  end

module SLContainer (S:Set.OrderedType) : (Container with type elt = S.t) =
  struct
    type t = S.t list
    type elt = S.t
    let empty = []
    let add el t =
      let rec insert l1 el = function
        | hd::tl when (S.compare hd el) < 0 -> insert (l1@[hd]) el tl
        | l2 -> l1 @ (el::l2)
      in insert [] el t
    let rec merge l1 l2 = match l1, l2 with
      | [], l -> l
      | l, [] -> l
      | (h1::t1), l2 when S.compare h1 (List.hd l2) < 0 ->  h1::(merge t1 l2)
      | l, (h2::t2) ->  h2::(merge l t2)
    let member el t =
      let rec mem_ord el = function
        | hd::tl when S.compare hd el <0 -> mem_ord el tl
        | hd::tl when S.compare hd el  = 0 -> true
        | _ -> false
      in mem_ord el t
    let fold f a t = List.fold_left f a t
end



let () =
  let module Test (M:Container with type elt = int) =
  struct
    open M
    let () =
      let s = add 42 (add 16 (add 64 empty)) in
      let s = merge s s in
      assert (member 42 s) ;
      assert (member 16 s) ;
      assert (member 64 s) ;
      Printf.printf "Result: " ;
      fold (fun () elt -> Printf.printf "%d+" elt) () s ;
      Printf.printf "ø\n"
  end
  in
  let module A = Test(LContainer(Int)) in
  let module B = Test(SLContainer(Int)) in ()



module type Printable = sig
  type t
  val to_string : t -> string
end

module type PContainer = sig
  include Container
  val to_string : t -> string
end

module PContainer (S:Set.OrderedType) : (Container with type elt = S.t) =
struct
  type t = S.t list
  type elt = S.t
  let empty = []
  let add el t =
    let rec insert l1 el = function
      | hd::tl when (S.compare hd el) < 0 -> insert (l1@[hd]) el tl
      | l2 -> l1 @ (el::l2)
    in insert [] el t
  let rec merge l1 l2 = match l1, l2 with
    | [], l -> l
    | l, [] -> l
    | (h1::t1), l2 when S.compare h1 (List.hd l2) < 0 ->  h1::(merge t1 l2)
    | l, (h2::t2) ->  h2::(merge l t2)
  let member el t =
    let rec mem_ord el = function
      | hd::tl when S.compare hd el <0 -> mem_ord el tl
      | hd::tl when S.compare hd el  = 0 -> true
      | _ -> false
    in mem_ord el t
  let fold f a t = List.fold_left f a t
end

(*
let () =
  let module Test (M:PContainer with type elt = string) =
  struct
    open M
    let () =
      let s =
        add "d" (merge (add "a" empty) (add "c" (add "b" empty)))
      in
      Printf.printf "Result: %s\n" (to_string s)
  end
  in
  let module A = Test(MakePrintable(String)(LContainer(String))) in
  let module B = Test(MakePrintable(String)(SLContainer(String))) in
  () *)
