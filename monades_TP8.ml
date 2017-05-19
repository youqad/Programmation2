module type MONAD = sig
  type +'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type EXN = sig
  include MONAD
  val throw : exn -> 'a t
  val try_with : 'a t -> (exn -> 'a t) -> 'a t
  val run : 'a t -> 'a   (* peut lever une exception *)
end


module Exn : EXN = struct
  type 'a t = Val of 'a | Exn of exn
  let return a = Val a
  let bind am f = match am with
    | Val a -> f a
    | Exn e -> Exn e
  let throw e = Exn e
  let try_with am f = match am with
    | Val a -> Val a
    | Exn e -> f e
  let run am = match am with
    | Val a -> a
    | Exn e -> raise e
end


let _ =
  let module M = Exn in
  let m =
    M.try_with
      (M.throw (Failure "normal"))
      (fun _ ->
         M.try_with
           (M.return 42)
           (fun _ -> M.throw (Failure "pas normal")))
  in
    Printf.printf "Test exn: %d\n" (M.run m)

type 'a cont = 'a -> unit

module type CONT = sig
  include MONAD
  val run : unit t -> unit
  val throw : 'a cont -> 'a -> 'b t
  val callcc : ('a cont -> 'a t) -> 'a t
end


module Cont : CONT = struct
  type 'a t = 'a cont -> unit
  let return a = fun k -> k a
  let bind (am : 'a t) (f : 'a -> 'b t) =
    fun (kb : 'b cont) -> am (fun a -> f a kb)
  let run k = k (fun () -> ())
  let throw k a = fun k' -> k a
  let callcc f = fun k -> f k k
end

let _ =
  let m = Cont.bind (Cont.return 21) (fun x -> Cont.return (2*x)) in
    Cont.run
      (Cont.bind m
         (fun x -> Printf.printf "Test cont: %d\n" x ; Cont.return ()))


let rec iter (f : ('a -> unit Cont.t)) (l:'a list) : unit Cont.t =
  match l with
  | [] -> Cont.return ()
  | a::l' -> Cont.bind (f a) (fun () -> iter f l')
