
(** Implémentation naïve de multi-ensembles par une liste d'associations.
  * Nous maintenons les invariants suivants:
  *  - un seul binding par valeur;
  *  - chaque entier est strictement positif. *)
type 'a multiset = ('a*int) list

(** Unique représentation du multi-ensemble vide. *)
let empty = []

(** [arity x l] donne le nombre d'occurrences de [x] dans [l]. *)
let arity x l =
  try List.assoc x l with Not_found -> 0

(** [add x l] renvoie un nouveau multi-ensemble
  * avec une occurrence de plus de [x]. *)
let add x l =
  let n = arity x l in
    (x,n+1) :: (List.remove_assoc x l)

(** [remove x l] renvoie un nouveau multi-ensemble
  * avec une occurrence de moins de [x]. *)
let remove x l =
  let n = arity x l in
    if n>1 then
      (x,n-1) :: (List.remove_assoc x l)
    else
      List.remove_assoc x l

(** Vérification de notre invariant. *)
let rec well_formed = function
  | [] -> true
  | (x,n) :: l ->
      n > 0 &&
      not (List.mem_assoc x l) &&
      well_formed l

(* Re-définition de [add] et [remove] avec vérification des invariants. *)

(** [add x l] renvoie un nouveau multi-ensemble
  * avec une occurrence de plus de [x]. *)
let add x l =
  assert (well_formed l) ;
  let l = add x l in
    assert (well_formed l) ;
    l

(** [remove x l] renvoie un nouveau multi-ensemble
  * avec une occurrence de moins de [x]. *)
let remove x l =
  assert (well_formed l) ;
  let l = remove x l in
    assert (well_formed l) ;
    l

(** Test *)
let () =
  assert (empty = remove 2 (remove 1 (add 2 (add 1 empty))))
