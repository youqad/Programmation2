#load "callcc.cma";;
open Callcc;;
let return = ref (Obj.magic None);;
let resume = ref (Obj.magic None);;

let fib () = callcc (fun kk -> return := kk;
                      let a,b = ref 1, ref 2 in
                      while true do
                        callcc (fun cc -> resume := cc; throw !return !a);
                        b := !a + !b; (* note: a,b ← b,a+b *)
                        a := !b - !a;
                      done; 0
                    )

