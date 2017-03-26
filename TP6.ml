let sum zero plus l =
  let rec f = function
    | [] -> zero
    | a::l -> plus a (f l)
  in f l

let () = assert ((sum 0 (+) [1;2;3]) = 6)

let () = assert ((sum "" (^) ["a";"b";"c"]) = "abc")

type ('a,'b) closure =
    { body : (('a,'b) closure * 'a) -> 'b;
      zero : 'b;
      plus : 'b->'b->'b }

let apply_closure c a = c.body (c,a)


let f_cl zero plus = {
  body = (fun (c,l) -> match l with
        | [] -> c.zero
        | h::t -> c.plus h (apply_closure c t));
  zero = zero;
  plus = plus
}


let sum zero plus l = apply_closure ( f_cl zero plus ) l

let _ = sum 0 (+) [1;2;3]
let _ = sum "" (^) ["a";"b";"c"]

(* class f_cl zero plus = *)
(*   object(self) *)
(*     method zero = zero *)
(*     method plus = plus *)
(*     method apply l = (match l with *)
(*       |  [] -> self#zero *)
(*       | a::l' -> self#plus a (self#apply l')) *)
(* end               *)

let rec rev l = match l with
  | [] -> []
  | a :: l -> ( rev l) @ [a]

let rec revk k l = match l with
  | [] -> k []
  | a::l -> let k' l' = k (l' @ [a]) in revk k' l

let _ = revk (fun l ->l) [1;2]



type 'a cont =
  | Id
  | C of ('a cont) * 'a

let rec apply_cont (cont: 'a cont) (l: 'a list) : 'a list =
  match cont with
  | Id -> l
  | C(k, a) -> apply_cont k (l@[a])

let rec revk2 (k: 'a cont) (l: 'a list) : 'a list = match l with
  | [] -> apply_cont k []
  | a::l' -> revk2 (C(k,a)) l'

let _ = revk2 Id [1;2]

 
