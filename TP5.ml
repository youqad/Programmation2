type cont = unit -> unit
type 'a success = 'a -> cont -> unit
type failure = cont
type 'a gen = 'a success -> failure -> unit

let stop : 'a gen = fun sk fk -> fk ()

let yield (a:'a) : 'a gen = fun sk fk ->
  sk a fk

let yield2 (a:'a) (b :'a): 'a gen = fun sk fk ->
  sk a (fun () -> sk b fk)


let iter (f:'a -> unit) (g:'a gen) =
  let sk a k : unit = f a ; k ()
  in
  let fk = fun () -> ()
  in
  g sk fk

let rec range (m:int) (n:int) : int gen =
  fun sk fk ->
    if m>=n then fk ()
    else
      sk m (fun () -> (range (m+1) n) sk fk)

let seq (g1 : 'a gen) (g2 : 'a gen) : 'a gen =
  fun sk fk -> g1 sk (fun () -> g2 sk fk)

let foreach (ga : 'a gen) (f: 'a -> 'b gen) : 'b gen =
  fun sk fk -> iter (fun a -> (f a) sk (fun () -> ())) ga; fk ()

let prod ga gb =
  foreach ga (fun a ->
      foreach gb (fun b ->
          yield (a, b)
        )
    )

let run (g: unit gen) =
  g (fun () k -> k ()) (fun () -> ())

(* let () =
   run (
    foreach (product (range 1 5) (range 1 5))
      (fun (i,j)->
         print_int i;
         print_int j;
        yield ()
      ) *)

(* let ()  *)
(*   foreach (range 1 5) (fun -> yield (i+1)) *)
(*         (fun i k -> print_int i; k ()) *)
(*         (fun () -> print_int ".") *)

(* let prod (ga : 'a gen) (gb : 'b gen) : ('a*'b) gen =
   fun sk fk -> let a, b = (ga sk fk, gb sk fk) in (a, b); *)


let _ = iter print_int (yield 3) (* doit afficher 3*)
let _ = iter print_int (yield2 1 2) (*doit afficher 12*)
let _ = iter print_int (range 1 10)
let _ = print_newline ()

let _ = iter print_int (seq (yield 1) (yield 2))

