
module type PRINTABLE = sig
  type t
  val to_string : t -> string
end

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Test (P : PRINTABLE) (O : ORDERED with type t = P.t) = struct
  let f x y =
    let cmp =
      match O.compare x y with
        | 0 -> '='
        | 1 -> '>'
        | _ -> '<'
    in
      Printf.printf "%s %c %s"
        (P.to_string x) cmp (P.to_string y)
end
