module type MONAD = sig
  type +'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end


module type ND_MONAD = sig
  include MONAD
  val plus : 'a t -> 'a t -> 'a t
  val fail : 'a t
  val run : 'a t -> ('a -> unit) -> unit
end


module ND_Monad : ND_MONAD = struct
  type 'a t = 'a list
  let return a = [a]
  let fail = []
  let bind am f = match am with
    | [] -> fail
    | l -> List.concat (List.map f l)
  let plus m n = m @ n
  let run am f = List.iter f am
end

let choose_some (l : 'a list) : 'a ND_Monad.t =
  List.fold_left
    (fun m x -> ND_Monad.plus m x) ND_Monad.fail
    (List.map (fun x -> ND_Monad.return x) l)

let choose_some2 (l : 'a list) : 'a ND_Monad.t =
  List.fold_left
    (fun m x -> ND_Monad.plus m (ND_Monad.return x))
    ND_Monad.fail l

let rec choose_some3 (l : 'a list) : 'a ND_Monad.t = match l with
  | [] -> ND_Monad.fail
  | h::t -> ND_Monad.plus (ND_Monad.return h) (choose_some3 l)

let _ = ND_Monad.run (choose_some [1; 2; 3]) print_int



module type ALEA_MONAD = sig
  include MONAD
  val choice : float -> 'a t -> 'a t -> 'a t
  val run : 'a t -> ('a -> float) -> float
end


module P : ALEA_MONAD = struct
  type 'a t = ('a -> float) -> float
  let return x = fun g -> g x
  let bind am f = fun g -> am (fun a -> f a g)
    (* tirer au hasard avec c, on obtient un a,
    considérer le calcul f a
    qui permet de tirer au hasard un b,
    évaluer le gain g sur ce b *)
  let choice p c1 c2 =
    fun g -> p *. (c1 g) +. (1.-.p) *. (c2 g)
  let run am f = am f
end


let pick (l : 'a list) : 'a P.t =
  let rec aux n l' = match l' with
  | [] -> raise Empty_list
  | [a] -> P.return a
  | a::l -> P.choice (1. /. (float n)) (P.return a) (aux (n-1) l) in
aux (List.length l) l

let dirac x = fun y -> if x=y then 1. else 0.
let proba m = m (dirac true)



let rec pick (l : 'a list) : ('a -> float) -> float =
  fun g -> let len = float (List.length l) in
    (List.fold_left
       (fun x y -> (g x) +. (g y)) 0. l)/. len
