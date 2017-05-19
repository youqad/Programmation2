module type MONAD = sig
  type + 'a m
  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end


module type PARSING = sig
  include MONAD
  val plus : 'a m -> 'a m -> 'a m
  val zero : 'a m
  val read : char m
  val eos : bool m
  val run : 'a m -> char list -> ( char list * 'a) list
end


module Parsing : PARSING = struct
  type 'a m = char list -> (char list * 'a) list
  let return (a : 'a) : 'a m = fun l -> [(l, a)]
  let bind (am : 'a m) (f : 'a -> 'b m) : 'b m = fun l ->
    List.concat (
      List.map (fun (l', a) -> f a l') (am l) )
  let plus (am : 'a m) (am' : 'a m) : 'a m = fun l ->
    List.concat [am l; am' l]
  let zero : 'a m = fun l -> []
  let read : char m = fun l ->
    match l with
    | [] -> []
    | h::t -> [t, h]
  let eos : bool m = fun l ->
    match l with
    | [] -> [[], true]
    | _ -> [l, false]
  let run (am : 'a m) = fun l -> am l
end

let (>>=) m f = Parsing.bind m f


let _ = Parsing.run (Parsing.plus Parsing.read (Parsing.bind Parsing.read (fun _ -> Parsing.read))) ['a'; 'b']

type word = char list

let one = fun l -> [[],[]]


let mk_parser (in_lang : char list -> bool) =
  let rec aux w = function
    | [] -> Parsing.zero
    | ch :: w' -> Parsing.plus
                    (if in_lang (w@[ch]) then Parsing.return (w@[ch]) else Parsing.zero)
                    (aux (w@[ch]) w') in
  aux

(*
   p(L) = w \mapsto \{ (w2, w1) | w = w1 w2 et w1 \in L \}

   M.return w1
*)

let char (c : char) : word Parsing.m =
  M.eos >>= (fun b -> if b=[] then Parsing.zero else
                Parsing.read >>= (fun ch2 -> ))
