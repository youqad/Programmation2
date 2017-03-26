(* class counter_with_dummy_id id = object
  val id = id
  val mutable c = 0
  method inc_get = c<- c+1; c
end

let counter1 = new counter_with_dummy_id 1;;

let counter2 = Oo.copy counter1;;

module M : MT = struct
  let c = ref 0

  class cp = object
    val c = c
    method inc_get = c<- c+1; c
  end
end


class counter_with_dummy_id = object
  val id = "dummy"
  val mutable c = 0
  method inc_get = c<- c+1; c
end


let rec fact x = if x = 0 then 1 else x * fact (x-1)
let f_fact fact x = if x = 0 then 1 else x * fact (x-1)

let rec make_rec f_phi x = f_phi (make_rec f_phi) x
let fact' = make_rec f_fact
let () =
  (* Test: fact et fact' coincident sur 1, 2, 3, 4. *)
  assert (List.fold_left (fun b i -> b && fact i = fact' i) true [1;2;3;4])


let make_rec_memo f_f =
  let table = Hashtbl.create 23 in
  let rec f x =
    try Hashtbl.find table x with
    | Not_found ->
      let r = f_f f x in
      Hashtbl.add table x r ;
      r
  in f

let fact'' = make_rec_memo f_fact
let () =
  (* Test: fact' et fact'' coincident sur 1, 2, 3, 4. *)
  assert (List.fold_left (fun b i -> b && fact' i = fact'' i) true [1;2;3;4])
 *)


class fibo =
  object (self)
    method compute = function
      | n when n < 2 -> 1
      | n -> (
          print_string ("compute "^(string_of_int (n-2))^" "^(string_of_int (n-1)));
          print_newline ();
          self#compute (n-1) + self#compute (n-2)
        )

end

class fibo_memo =
  object (self)
  inherit fibo as super
  val table = Hashtbl.create 100
  method compute = function
      | n when n < 2 -> 1
      | n -> (
        try Hashtbl.find table n with
        | Not_found ->
          let fibn = super#compute n in
          Hashtbl.add table n fibn; fibn
        )
end


let test1 = new fibo;;

let test2 = new fibo_memo;;

print_int (test1#compute 10);;

print_newline ();;

print_int (test2#compute 10);;

module type ClassCompute = sig
  class c : object
    method compute : int -> int
  end
end


module MakeMemo (M:ClassCompute) : ClassCompute = struct
  class c =
    object (self)
    inherit M.c as super
    val table = Hashtbl.create 100
    method compute = function
        | n when n < 2 -> 1
        | n -> (
            try Hashtbl.find table n with
            | Not_found ->
              let fibn = super#compute n in
              Hashtbl.add table n fibn; fibn
          )
    end
end


module FiboClass = struct class c = fibo end;;

module M = MakeMemo(FiboClass);;

class fibo_memo2 = M.c;;

let test3 = new fibo_memo2;;

print_int (test3#compute 10);;

print_newline ();;
