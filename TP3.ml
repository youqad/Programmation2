class ['a] ostring (s:string) = object (self)
  val s = s
  method get = s
  method concat (s2:'a) = new ostring (String.concat "" [s; s2#get])
end

class ['a] ostring2 s = object (self)
    inherit ['a] ostring s as super
    method len = String.length self#get
end

class ['a] ostring3 s = object (self)
    inherit ['a] ostring2 s as super
    method index ?(rev=false) ?ind c = let s = (self#get) in
        match rev with 
        | false -> (match ind with 
            | None -> String.index_from s 0 c
            | Some n -> String.index_from s n c)
        | _ -> (match ind with 
            | None -> String.rindex_from s (String.length s - 1) c
            | Some n -> String.rindex_from s n c)
end


(*let s1 = new ostring "1"
let s2 = new ostring2 "2"
let _ = s1#concat s2

let f ?a b = match a with
    | None -> b
    | Some aa -> aa + b*)



(*let h ?(a=0) b = a+b

let _ = f 3

let _ = f ~a:4 3

h ~a:2 3*)

(********************* Q3 ******************************)

let s = new ostring3 "abab";;
let _ = s#index 'a'         (* = 0 *);;
let _ = s#index 'a' ~ind:1       (* = 2 *);;
let _ = s#index 'a' ~rev:true    (* = 2 *);;
let _ = s#index 'a' ~rev:true ~ind:3 (* = 2 *);;


(********************* EX 2 ******************************)

type 'a str
type clean
type dirty

(* On peut calculer la longueur de toute chaine,
 * les inputs sont sales (on se mefie de l'utilisateur)
 * et doivent etres nettoyes avant affichage. *)


type 'a str = string
type clean = unit (* ou autre, qu'importe *)
type dirty = unit

let length s = String.length s
let read () = read_line ()
let write s = Printf.printf "%s\n" s
let sanitize s = Filename.quote s

(*******************************************)

